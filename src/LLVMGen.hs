{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LLVMGen where

import Data.Word
import Data.String
import Data.List
import Data.Function
import qualified Data.Map as Map

import Control.Monad.State
import Control.Applicative

import LLVM.Prelude
import LLVM.AST.Global

import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.IntegerPredicate as IP

-- MODULES

-- | State monad to build up a LLVM module as the AST of the source language
-- is traversed
newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module)

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM mod (LLVM m) = execState m mod

-- | Create a named empty module
emptyModule :: ShortByteString -> AST.Module
emptyModule fileName = AST.defaultModule {
    AST.moduleName = fileName
  , AST.moduleSourceFileName = fileName
  }

-- | Append a new declaration to the current module
addDefn :: AST.Definition -> LLVM ()
addDefn d = do
  modify $ \s -> s { AST.moduleDefinitions = AST.moduleDefinitions s ++ [d] }

-- | Define a new function
define :: AST.Type
          -> ShortByteString
          -> [(AST.Type, AST.Name)]
          -> [BasicBlock]
          -> LLVM ()
define retty label argtys body = addDefn $
  AST.GlobalDefinition $ functionDefaults {
    name        = AST.Name label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = body
  }

-- Declare a new external function
external :: AST.Type
            -> ShortByteString
            -> [(AST.Type, AST.Name)]
            -> LLVM ()
external retty label argtys = addDefn $
  AST.GlobalDefinition $ functionDefaults {
    name        = AST.Name label
  , linkage     = L.External
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = []
  }

-- FUNCTION GENERATION STATE

-- | Symbol table to allow for multiple scopings within the same function
data ScopedSymbolTable
  = ScopedSymbolTable {
    -- Parent Symbol Table
    parentTable :: Maybe ScopedSymbolTable
  , symbolTable :: Map.Map ShortByteString AST.Operand
  } deriving (Show, Eq)

-- | An empty, root scoped symboled table
emptyScopedSymbolTable :: ScopedSymbolTable
emptyScopedSymbolTable = ScopedSymbolTable Nothing Map.empty

pushScope :: FunctionGen ()
pushScope = do
  syms <- gets scopedSymbolTable
  let syms' = emptyScopedSymbolTable { parentTable = Just syms }
  modify $ \s -> s { scopedSymbolTable = syms' }

popScope :: FunctionGen ()
popScope = do
  syms <- gets scopedSymbolTable
  case parentTable syms of
    Just syms' -> modify $ \s -> s { scopedSymbolTable = syms' }
    Nothing -> return () -- TODO throw error?

data FunctionGenState
  = FunctionGenState {
    -- Name of the active block to append to
    currentBlock :: AST.Name
    -- Blocks for function
  , blocks :: Map.Map AST.Name BlockState
    -- Function scope symbol table
  , scopedSymbolTable :: ScopedSymbolTable
    -- Count of basic blocks
  , blockCount :: Int
    -- Count of unnamed instructions, used to get the next free number
  , instrCount :: Word
    -- Map from names to number of times it has been used, used to get fresh
    -- names
  , names :: Map.Map ShortByteString Int
    -- Map from function names to their compiled types
  , ftyCtxt :: Map.Map ShortByteString AST.Type
    -- Stack of hoisted instructions
  , hoistedStack :: [AST.Named AST.Instruction]
  } deriving (Show, Eq)

data BlockState
  = BlockState {
    -- Block index
    idx :: Int
    -- Stack of instructions
  , instrStack :: [AST.Named AST.Instruction]
    -- Block terminator
  , term :: Maybe (AST.Named AST.Terminator)
  } deriving (Show, Eq)

-- FUNCTION GENERATION

-- | State monad to build up a function's blocks as the AST of the source
-- language is traversed
newtype FunctionGen a =
  FunctionGen { runFunctionGen :: State FunctionGenState a}
  deriving (Functor, Applicative, Monad, MonadState FunctionGenState )

-- | Extract the generated function out of its state monad
execFunctionGen :: FunctionGen a -> FunctionGenState
execFunctionGen m = execState (runFunctionGen m) emptyFunctionGen where
  -- | Create a new FunctionGen with the current block set as the entry block
  emptyFunctionGen :: FunctionGenState
  emptyFunctionGen = FunctionGenState
    (AST.Name entryBlockName)
    Map.empty
    emptyScopedSymbolTable
    1
    0
    Map.empty
    Map.empty
    []

-- | Name of the first block in all functions
entryBlockName :: ShortByteString
entryBlockName = "entry"

-- | Transform a FunctionState into
createBlocks :: FunctionGenState -> [BasicBlock]
createBlocks m =
  let aggBlocks = Map.adjust
                    (\b -> b { instrStack = instrStack b ++ hoistedStack m })
                    (AST.Name entryBlockName)
                    (blocks m) in
  map makeBlock $ sortBlocks $ Map.toList aggBlocks
  where
    -- Sort by the block index
    sortBlocks :: [(AST.Name, BlockState)] -> [(AST.Name, BlockState)]
    sortBlocks = sortBy (compare `on` (idx . snd))
    -- | Transform a BlockState into a BasicBlock with a given name
    makeBlock :: (AST.Name, BlockState) -> BasicBlock
    makeBlock (l, (BlockState _ s (Just t))) = BasicBlock l (reverse s) t
    makeBlock (l, (BlockState _ _ Nothing)) =
      error $ "Block has no terminator: " ++ (show l)

-- | Create an empty block with the given index
emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

-- | Adds a block with the given name
addBlock :: ShortByteString -> FunctionGen AST.Name
addBlock blockName = do
  bls <- gets blocks
  count <- gets blockCount
  name' <- freshName blockName
  modify $ \s -> s { blocks = Map.insert (AST.Name name') (emptyBlock count) bls
                   , blockCount = count + 1
                   }
  return $ AST.Name name'

-- | Sets the current block name to the given name
setCurrentBlock :: AST.Name -> FunctionGen ()
setCurrentBlock blockName = do
  modify $ \s -> s { currentBlock = blockName }

-- | Sets the contents of the current block to the given state
modifyBlock :: BlockState -> FunctionGen ()
modifyBlock b = do
  activeName <- getCurrentBlockName
  modify $ \s -> s { blocks = Map.insert activeName b (blocks s) }

-- | Returns the name of the current block
getCurrentBlockName :: FunctionGen AST.Name
getCurrentBlockName = gets currentBlock

-- | Returns the contents of the current block
getCurrentBlock :: FunctionGen BlockState
getCurrentBlock = do
  c <- getCurrentBlockName
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block: " ++ show c

-- | Removes the given block from the function generator statex
removeBlock :: AST.Name -> FunctionGen ()
removeBlock name = do
  blks <- gets blocks
  modify $ \s -> s { blocks = Map.delete name blks }

-- | Get a fresh number to use for unnamed statements
freshUnName :: FunctionGen Word
freshUnName = do
  i <- gets instrCount
  modify $ \s -> s { instrCount = 1 + i }
  return $ i + 1

-- | Converts a name into a unique name
freshName :: ShortByteString -> FunctionGen ShortByteString
freshName name = do
  ns <- gets names
  let (name', ns') = uniqueName name ns
  modify $ \s -> s { names = ns' }
  return name'
  where
    uniqueName name names = case Map.lookup name names of
      Just i  -> (name <> fromString (show i), Map.insert name (i + 1) names)
      Nothing -> (name, Map.insert name 1 names)

-- | Append a new instruction to the current block
instr :: AST.Type -> AST.Instruction -> FunctionGen (AST.Operand)
instr ty ins = do
  n <- freshUnName
  let ref = AST.UnName n
  block <- getCurrentBlock
  let instrs = instrStack block
  modifyBlock (block { instrStack = (ref AST.:= ins) : instrs } )
  return $ local ty ref

-- | Append a new instruction to the hoisted stack of the function
hoistedInstr :: AST.Type -> AST.Instruction -> FunctionGen (AST.Operand)
hoistedInstr ty ins = do
  n <- freshUnName
  let ref = AST.UnName n
  hoisted <- gets hoistedStack
  modify $ \s -> s { hoistedStack = (ref AST.:= ins) : hoisted }
  return $ local ty ref

-- | Append a new instruction to the current block without saving a result value
voidInstr :: AST.Instruction -> FunctionGen ()
voidInstr ins = do
  block <- getCurrentBlock
  let instrs = instrStack block
  modifyBlock (block { instrStack = (AST.Do ins) : instrs } )

-- | Set the terminator for the current block
terminator :: AST.Named AST.Terminator -> FunctionGen ()
terminator term = do
  block <- getCurrentBlock
  modifyBlock (block { term = Just term })

setFtyCtxt :: Map.Map ShortByteString AST.Type -> FunctionGen()
setFtyCtxt ctxt = do
  modify $ \s -> s { ftyCtxt = ctxt }

-- SYMBOL TABLE

-- | Assign a name to the given LLVM operand
assign :: ShortByteString -> AST.Operand -> FunctionGen ()
assign name x = do
  v <- getVarInCurrentScope name
  case v of
    Just _ -> error $ "Cannot redeclare variable " ++ show name
    Nothing -> do
      syms <- gets scopedSymbolTable
      let syms' = syms { symbolTable = Map.insert name x (symbolTable syms) }
      modify $ \s ->
        s { scopedSymbolTable = syms'}
  where
    -- | Get the LLVM operand associated with the given string in the current
    -- scope
    getVarInCurrentScope :: ShortByteString ->
                            FunctionGen (Maybe AST.Operand)
    getVarInCurrentScope name = do
      syms <- gets scopedSymbolTable
      return $ case Map.lookup name (symbolTable syms) of
        Just x  -> Just x
        Nothing -> Nothing

-- | Get the LLVM operand associated with the given variable name in any of the
-- current or parent scopes
getVar :: ShortByteString -> FunctionGen (Maybe AST.Operand)
getVar name = do
  syms <- gets scopedSymbolTable
  getVarInScope name syms where
    getVarInScope :: ShortByteString -> ScopedSymbolTable ->
                     FunctionGen (Maybe AST.Operand)
    getVarInScope name syms =
      case Map.lookup name (symbolTable syms) of
        Just x  -> return $ Just x
        Nothing -> case parentTable syms of
          Just p -> getVarInScope name p
          Nothing -> return Nothing

-- | Get the LLVM type associated with the given function name
getFunctionType :: ShortByteString -> FunctionGen (Maybe AST.Type)
getFunctionType name = do
  ctxt <- gets ftyCtxt
  return $ case Map.lookup name ctxt of
    Just ty -> Just ty
    Nothing -> Nothing

-- | Get an LLVM operand pointing to the function with the given name
getFunction :: ShortByteString -> FunctionGen (Maybe AST.Operand)
getFunction name = do
  ty <- getFunctionType name
  return $ case ty of
    Just ty' ->
      Just $ AST.ConstantOperand $ C.GlobalReference ty' $ AST.Name name
    Nothing -> Nothing

localv :: ShortByteString -> FunctionGen AST.Operand
localv name = do
  v <- getVar name
  return $ case v of
    Just op -> op
    Nothing -> error $ "Local variable not in scope: " ++ show name

globalf :: ShortByteString -> FunctionGen AST.Operand
globalf name = do
  f <- getFunction name
  return $ case f of
    Just op -> op
    Nothing -> error $ "Function " ++ show name ++ " not found"

-- REFERENCES

local :: AST.Type -> AST.Name -> AST.Operand
local = AST.LocalReference

global :: AST.Type -> AST.Name -> C.Constant
global = C.GlobalReference

-- TYPES

booleanSize :: Word32
booleanSize = 1

integerSize :: Word32
integerSize = 64

double :: AST.Type
double = AST.FloatingPointType AST.DoubleFP

integer :: AST.Type
integer = AST.IntegerType integerSize

boolean :: AST.Type
boolean = AST.IntegerType booleanSize

-- Variable numbers of arguments is not allowed
functionPtr :: AST.Type -> [AST.Type] -> AST.Type
functionPtr retty argtys = T.ptr $ AST.FunctionType retty argtys False

void :: AST.Type
void = AST.VoidType

typeFromOperand :: AST.Operand -> AST.Type
typeFromOperand (AST.LocalReference t@(AST.IntegerType 1) _) = t
typeFromOperand (AST.LocalReference t@(AST.IntegerType 64) _) = t
typeFromOperand (AST.LocalReference t@(AST.FloatingPointType AST.DoubleFP) _) =
  t
typeFromOperand (AST.LocalReference t@(AST.PointerType (AST.FunctionType _ _ _) _) _) = t
typeFromOperand (AST.ConstantOperand (C.Int 64 _)) = integer
typeFromOperand (AST.ConstantOperand (C.Int 1 _)) = boolean
typeFromOperand (AST.ConstantOperand (C.GlobalReference t _)) = t
typeFromOperand e = error $ "Unrecognized type: " ++ show e

-- CONSTANTS

cons :: C.Constant -> AST.Operand
cons = AST.ConstantOperand

-- | Create a constant operand with the given integer value
integerConst :: Integer -> AST.Operand
integerConst i = cons $ C.Int integerSize i

-- | Create a constant operand with the given float value
doubleConst :: Double -> AST.Operand
doubleConst i = cons $ C.Float $ F.Double i

-- | Create a constant operand with the given boolean value
booleanConst :: Bool -> AST.Operand
booleanConst b = cons $ C.Int booleanSize $ toInteger $ fromEnum b

-- OPERATORS

-- | Float addition
fadd :: AST.Operand -> AST.Operand -> FunctionGen AST.Operand
fadd a b = instr double $ AST.FAdd AST.noFastMathFlags a b []

-- | Float subtraction
fsub :: AST.Operand -> AST.Operand -> FunctionGen AST.Operand
fsub a b = instr double $ AST.FSub AST.noFastMathFlags a b []

-- | Float multiplication
fmul :: AST.Operand -> AST.Operand -> FunctionGen AST.Operand
fmul a b = instr double $ AST.FMul AST.noFastMathFlags a b []

-- | Float division
fdiv :: AST.Operand -> AST.Operand -> FunctionGen AST.Operand
fdiv a b = instr double $ AST.FDiv AST.noFastMathFlags a b []

-- | Float comparision
fcmp :: FP.FloatingPointPredicate -> AST.Operand -> AST.Operand
        -> FunctionGen AST.Operand
fcmp cond a b = instr double $ AST.FCmp cond a b []

-- | Float negation
fneg :: AST.Operand -> FunctionGen AST.Operand
fneg a = instr double $ AST.FSub AST.noFastMathFlags (doubleConst 0) a []

-- | Integer addition
iadd :: AST.Operand -> AST.Operand -> FunctionGen AST.Operand
iadd a b = instr integer $ AST.Add False False a b []

-- | Integer subtraction
isub :: AST.Operand -> AST.Operand -> FunctionGen AST.Operand
isub a b = instr integer $ AST.Sub False False a b []

-- | Integer multiplication
imul :: AST.Operand -> AST.Operand -> FunctionGen AST.Operand
imul a b = instr integer $ AST.Mul False False a b []

-- | Integer division
idiv :: AST.Operand -> AST.Operand -> FunctionGen AST.Operand
idiv a b = instr integer $ AST.SDiv False a b []

-- | Integer comparisons
icmp :: IP.IntegerPredicate -> AST.Operand -> AST.Operand
        -> FunctionGen AST.Operand
icmp cond a b = instr integer $ AST.ICmp cond a b []

-- | Integer left shift
ilshift :: AST.Operand -> AST.Operand -> FunctionGen AST.Operand
ilshift a b = instr integer $ AST.Shl False False a b []

-- | Integer right shift (arithmatic)
irshift :: AST.Operand -> AST.Operand -> FunctionGen AST.Operand
irshift a b = instr integer $ AST.AShr False a b []

-- | Integer bitwise and
iand :: AST.Operand -> AST.Operand -> FunctionGen AST.Operand
iand a b = instr integer $ AST.And a b []

-- | Integer bitwise or
ior :: AST.Operand -> AST.Operand -> FunctionGen AST.Operand
ior a b = instr integer $ AST.Or a b []

-- | Integer modulo
imod :: AST.Operand -> AST.Operand -> FunctionGen AST.Operand
imod a b = instr integer $ AST.SRem a b []

-- | Integer negation
ineg :: AST.Operand -> FunctionGen AST.Operand
ineg a = instr integer $ AST.Sub False False (integerConst 0) a []

-- | Boolean eq
beq :: AST.Operand -> AST.Operand -> FunctionGen AST.Operand
beq a b = instr boolean $ AST.ICmp IP.EQ a b []

-- | Boolean neq
bneq :: AST.Operand -> AST.Operand -> FunctionGen AST.Operand
bneq a b = instr boolean $ AST.ICmp IP.NE a b []

-- | Boolean and
band :: AST.Operand -> AST.Operand -> FunctionGen AST.Operand
band a b = instr boolean $ AST.And a b []

-- | Boolean or
bor :: AST.Operand -> AST.Operand -> FunctionGen AST.Operand
bor a b = instr boolean $ AST.Or a b []

-- | Boolean not
bnot :: AST.Operand -> FunctionGen AST.Operand
bnot a = instr boolean $ AST.Xor (booleanConst True) a []

-- EXPRESSIONS

-- Adds empty parameter attributes
toArgs :: [AST.Operand] -> [(AST.Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

-- Call the given function with the given arguments
call :: AST.Operand -> [AST.Operand] -> AST.Type -> FunctionGen AST.Operand
call fun args retty =
  instr retty $ AST.Call Nothing CC.C [] (Right fun) (toArgs args) [] [] where

-- Call the given function with the given arguments
scall :: AST.Operand -> [AST.Operand] -> FunctionGen ()
scall fun args =
  voidInstr $ AST.Call Nothing CC.C [] (Right fun) (toArgs args) [] [] where

-- | Allocate space for a local variable of the given type
alloca :: AST.Type -> FunctionGen AST.Operand
alloca ty = hoistedInstr ty $ AST.Alloca ty Nothing 0 []

-- | Store the given value at the given pointer
store :: AST.Operand -> AST.Operand -> FunctionGen ()
store val ptr = voidInstr $ AST.Store False ptr val Nothing 0 []

-- | Load value stored at the given pointer
load :: AST.Operand -> FunctionGen AST.Operand
load ptr = instr (typeFromOperand ptr) $ AST.Load False ptr Nothing 0 []

-- CONTROL FLOW

currentBlockHasRet :: FunctionGen Bool
currentBlockHasRet = do
  block <- getCurrentBlock
  return $ case term block of
    Just (AST.Do (AST.Ret _ _)) -> True
    _ -> False

-- Branch, if no Ret has been set
br :: AST.Name -> FunctionGen ()
br val = do
  h <- currentBlockHasRet
  case h of
    True -> return ()
    False -> terminator $ AST.Do $ AST.Br val []

-- Contiditonal branch, if no Ret has been set
cbr :: AST.Operand -> AST.Name -> AST.Name -> FunctionGen ()
cbr cond tr fl = do
  h <- currentBlockHasRet
  case h of
    True -> return ()
    False -> terminator $ AST.Do $ AST.CondBr cond tr fl []

-- Return
ret :: AST.Operand -> FunctionGen ()
ret val = terminator $ AST.Do $ AST.Ret (Just val) []


-- FUNCTION TYPE EXTRACTION

newtype FunctionTypeGen a =
  FunctionTypeGen { runFunctionTypeGen :: State FunctionTypeContext a}
  deriving (Functor, Applicative, Monad, MonadState FunctionTypeContext )

type FunctionTypeContext = Map.Map ShortByteString AST.Type

-- | Extract the generated function out of its state monad
execFunctionTypeGen :: FunctionTypeGen a -> FunctionTypeContext
execFunctionTypeGen m = execState (runFunctionTypeGen m) Map.empty

setType :: ShortByteString -> AST.Type -> FunctionTypeGen ()
setType id ty = do
  s <- get
  case Map.lookup id s of
    Just _ -> error $ "Cannot have multiple definitions of function " ++ show id
    Nothing -> modify $ \s -> Map.insert id ty s