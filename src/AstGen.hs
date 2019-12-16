{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AstGen where

import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Data.Text (pack, unpack)

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Identity

import Test.QuickCheck
import Test.QuickCheck as QC
import Test.QuickCheck.Gen
import Test.QuickCheck.StringRandom (matchRegexp)
import qualified QuickCheck.GenT as QCT
import qualified Data.Map as Map

import Ast
import PrettyAst

--------------------------------------
-- UTILS - ---------------------------
--------------------------------------

-- | Print 10 programs to the stdout
run :: IO ()
run = sample $ progGen

-- | Output a program in the IO Monad
run' :: IO Prog
run' = generate $ progGen

-- Data Types

-- | Name, arg types and names, return type (must not be void)
data FunTy = FunTy Id [(Ty, Id)] Ty deriving Show
-- | Name, arg types, return type (used to generate call expressions)
data CallTy = CallTy Id [Ty] Ty deriving Show

-- Entry Points

instance Arbitrary Prog where
  arbitrary = progGen

mainTy :: FunTy
mainTy = FunTy "main" [] TInt

-- | Top Level Generator for Prog data type
progGen :: Gen Prog
progGen = do
  numFuns <- choose (0, 2) -- 3 functions cause stack overflow
  ftys <- vectorOf numFuns genFunTy
  execAstGenerator mainTy ftys

-- | Execute the generator given a main funty and a list of other funtys
execAstGenerator :: FunTy -> [FunTy] -> Gen Prog
execAstGenerator main fs =
  let contextGen = execStateT genProg (initGlobalContext main fs) in
  fmap (Prog . (map (Gfdecl . noLoc . fCtxtToFdecl)) . Map.elems . contexts) contextGen
  where
    fCtxtToFdecl :: FunctionContext -> Fdecl
    fCtxtToFdecl (FunctionContext fty (FunctionSubContext Nothing _ _ _ block)) = funTytoFdecl fty (reverse block)
    fCtxtToFdecl _ = error "Subcontext is not root"
    funTytoFdecl :: FunTy -> Block -> Fdecl
    funTytoFdecl (FunTy fn args retty) b = Fdecl (RetVal retty) fn args b

--------------------------------------
-- CONTEXT ---------------------------
--------------------------------------

-- State data structures

data GlobalContext
  = GlobalContext {
    -- Function currently being generated
    currentFun :: Id
    -- Map from function name to its context
  , contexts :: Map.Map Id FunctionContext
    -- Map from retty to function type
  , funs :: Map.Map Ty [FunTy]
    -- Next integer to append to variable name for unique ids
  , nextId :: Int
  } deriving Show

data FunctionContext
  = FunctionContext {
    -- Type for this function
    fty :: FunTy
    -- Subcontext stack
  , currSubCtxt :: FunctionSubContext
  } deriving Show

data FunctionSubContext
  = FunctionSubContext {
    -- Parent context
    parentSubCtxt :: Maybe FunctionSubContext
    -- Declared vars
  , vars :: Map.Map Id Ty
    -- Map from each type to vars of that type declared in this scope
  , tysToVars :: Map.Map Ty [Id]
    -- Map from return type to functions of that return type declared in this scope
  , tysToFuns :: Map.Map Ty [CallTy]
    -- Accumulated statements in this scope
  , stmts :: Block
  } deriving Show

-- Instance so we can move between QCT.MonadGen and StateT s Gen

instance QCT.MonadGen (StateT s Gen) where
  liftGen = lift . QCT.liftGen
  variant = mapStateT . QCT.variant
  sized f = StateT $ \state -> sized $ \size -> runStateT (f size) state
  resize = mapStateT . QCT.resize
  choose = lift . QCT.choose

-- Convienience Initializers

-- | Intializes an empty function sub context with no parent
emptyFunctionSubContext :: FunctionSubContext
emptyFunctionSubContext = FunctionSubContext Nothing Map.empty Map.empty Map.empty []

-- | Intializes an empty function context with no parent, initalizes declared
-- variables with the arguments
emptyFunctionContext :: FunTy -> FunctionContext
emptyFunctionContext fty@(FunTy _ args _) =
  FunctionContext fty emptyFunctionSubContext {
      vars = Map.fromList $ map swap args
    , tysToVars = foldr (\(ty, id) acc -> Map.alter (Just [id] <>) ty acc) Map.empty args
    , tysToFuns = foldr foldInCallTy Map.empty args
    }
  where
    foldInCallTy :: (Ty, Id) -> Map.Map Ty [CallTy] -> Map.Map Ty [CallTy]
    foldInCallTy (ty, id) acc = case ty of
                       TRef (RFun aTys (RetVal rty)) -> Map.alter (Just [CallTy id aTys rty] <>) rty acc
                       _                    -> acc

-- | Intializes an empty global context
initGlobalContext :: FunTy -> [FunTy] -> GlobalContext
initGlobalContext main@(FunTy fn _ _) fs =
  let fs' = main : fs in
  let fds = foldr (\f@(FunTy _ _ retty) acc -> Map.alter (Just [f] <>) retty acc) Map.empty fs -- no main
      ctxts = Map.fromList $
        map (\f@(FunTy fn _ _) -> (fn, emptyFunctionContext f)) fs'
  in GlobalContext fn ctxts fds 0

-- Helper functions

getNextInt :: (QCT.MonadGen m, MonadState GlobalContext m) => m Int
getNextInt = do
 id <- gets nextId
 modify $ \s -> s { nextId = id + 1 }
 return id

currentFunctionContext :: (QCT.MonadGen m, MonadState GlobalContext m) =>
                          m FunctionContext
currentFunctionContext = do
  c <- gets currentFun
  ctxts <- gets contexts
  case Map.lookup c ctxts of
    Just x -> return x
    Nothing -> error $ "No such function context: " ++ show c

-- | Sets the current function name to the given name
setCurrentFun :: (QCT.MonadGen m, MonadState GlobalContext m) => Id -> m ()
setCurrentFun funName = do
  modify $ \s -> s { currentFun = funName }

-- | Sets the contents of the current function context to the given state
modifyCurrentFunctionContext :: (QCT.MonadGen m, MonadState GlobalContext m) =>
                                FunctionContext -> m ()
modifyCurrentFunctionContext f = do
  activeName <- gets currentFun
  modify $ \s -> s { contexts = Map.insert activeName f (contexts s) }

-- | Converts a FunTy to a CallTy
funTyToCallTy :: FunTy -> CallTy
funTyToCallTy f@(FunTy id args retty) = CallTy id (map fst args) retty

-- | Looks for functions with the argued return type
inScopeCallTysWithRetType :: (QCT.MonadGen m, MonadState GlobalContext m) => Ty -> m [CallTy]
inScopeCallTysWithRetType ty = do
  fs   <- gets funs
  curr <- currentFunctionContext
  let globalFuns = Map.findWithDefault [] ty fs
  return $ (accFuns $ currSubCtxt curr) ++ (map funTyToCallTy globalFuns)
  where
    -- | looks up local functions
    accFuns :: FunctionSubContext -> [CallTy]
    accFuns c =
      let pfuns = case parentSubCtxt c of
                    Just c' -> accFuns c'
                    Nothing -> []
          cfuns = Map.findWithDefault [] ty (tysToFuns c) in
      cfuns ++ pfuns

-- | Looks for global functions with the argued return type
globalFunsWithRetty :: (QCT.MonadGen m, MonadState GlobalContext m) => Ty -> m [FunTy]
globalFunsWithRetty ty = do
  fs <- gets funs
  case Map.lookup ty fs of
    Nothing     -> return []
    Just funtys -> return funtys

-- | Looks recursively up the context stack and merges var decls
inScopeVars :: (QCT.MonadGen m, MonadState GlobalContext m) => m (Map.Map Id Ty)
inScopeVars = do
  curr <- currentFunctionContext
  return $ accVars $ currSubCtxt curr
  where
    accVars :: FunctionSubContext -> Map.Map Id Ty
    accVars c =
      let pvars = case parentSubCtxt c of
                    Just c' -> accVars c'
                    Nothing -> Map.empty in
      Map.union (vars c) pvars

-- | Looks recursively up the context stack and
inScopeTysToVars :: (QCT.MonadGen m, MonadState GlobalContext m) =>
                    m (Map.Map Ty [Id])
inScopeTysToVars = do
  curr <- currentFunctionContext
  return $ accVars $ currSubCtxt curr
  where
    accVars :: FunctionSubContext -> Map.Map Ty [Id]
    accVars c =
      let pvars = case parentSubCtxt c of
                    Just c' -> accVars c'
                    Nothing -> Map.empty in
      mergeMaps pvars (tysToVars c)
    mergeMaps :: Map.Map Ty [Id] -> Map.Map Ty [Id] -> Map.Map Ty [Id]
    mergeMaps m1 m2 =
      Map.foldrWithKey (\k v acc -> Map.alter (Just v <>) k acc) m1 m2

-- | Looks recursively up the context stack and returns closest found value
inScopeVarsWithType :: (QCT.MonadGen m, MonadState GlobalContext m) =>
                       Ty -> m [Id]
inScopeVarsWithType ty = do
  curr <- currentFunctionContext
  return $ accVars ty $ currSubCtxt curr
  where
    accVars :: Ty -> FunctionSubContext -> [Id]
    accVars ty c =
      fromMaybe [] $ (Map.lookup ty (tysToVars c)) <|> (fmap (accVars ty) (parentSubCtxt c))

-- | Adds a new binding for the given Id in the current context
declVar :: (QCT.MonadGen m, MonadState GlobalContext m) =>
           Id -> Ty -> m ()
declVar x ty = do
  curr <- currentFunctionContext
  let currSub = currSubCtxt curr
  let vars' = Map.insert x ty (vars currSub)
  let tysToVars' = Map.alter (Just [x] <>) ty (tysToVars currSub)
  modifyCurrentFunctionContext curr {
    currSubCtxt = currSub {
      vars = vars', tysToVars = tysToVars' } }

-- | Creates a new context with the current as its parent with the same name
pushSubContext :: (QCT.MonadGen m, MonadState GlobalContext m) => m ()
pushSubContext = do
  curr <- currentFunctionContext
  let currSub = currSubCtxt curr
  let currSub' = emptyFunctionSubContext { parentSubCtxt = Just currSub }
  modifyCurrentFunctionContext curr { currSubCtxt = currSub' }

-- | Sets the current context as the current parent and returns the accumulated
-- block
popSubContext :: (QCT.MonadGen m, MonadState GlobalContext m) => m Block
popSubContext = do
  curr <- currentFunctionContext
  let currSub = currSubCtxt curr
  case parentSubCtxt currSub of
    Just currSub' ->
      modifyCurrentFunctionContext curr { currSubCtxt = currSub' }
    Nothing -> error "Cannot pop top level subcontext"
  return $ reverse $ stmts currSub

pushStmt :: (QCT.MonadGen m, MonadState GlobalContext m) => Stmt -> m ()
pushStmt s = do
  curr <- currentFunctionContext
  let currSub = currSubCtxt curr
  let currSub' = currSub { stmts = noLoc s : (stmts currSub) }
  modifyCurrentFunctionContext curr { currSubCtxt = currSub' }


--------------------------------------
-- GENERATORS ------------------------
--------------------------------------

-- Operator Generators

genBoolUnop :: (QCT.MonadGen m, MonadState GlobalContext m) => m Unop
genBoolUnop = QCT.liftGen $ return Ast.Lognot

genBoolBinop :: (QCT.MonadGen m, MonadState GlobalContext m) => m Binop
genBoolBinop = QCT.liftGen $ elements [Ast.And, Ast.Or]

genIntUnop :: (QCT.MonadGen m, MonadState GlobalContext m) => m Unop
genIntUnop = QCT.liftGen $ return Ast.Neg

genIntBinop :: (QCT.MonadGen m, MonadState GlobalContext m) => m Binop
genIntBinop = QCT.liftGen $ elements [Ast.Add, Ast.Sub, Ast.Mul, Ast.IAnd, Ast.IOr, Ast.Shl, Ast.Shr]

genOrdCompBinop :: (QCT.MonadGen m, MonadState GlobalContext m) => m Binop
genOrdCompBinop = QCT.liftGen $ elements [Ast.Lt, Ast.Lte, Ast.Gt, Ast.Gte]

genEqCompBinop :: (QCT.MonadGen m, MonadState GlobalContext m) => m Binop
genEqCompBinop = QCT.liftGen $ elements [Ast.Eq, Ast.Neq]

-- Expression Generators

-- | Generates an expresssion of an argued type
genExpWithType :: (QCT.MonadGen m, MonadState GlobalContext m) => Ty -> m Exp
genExpWithType ty = case ty of
                  TBool -> genBoolExp
                  TInt  -> genIntExp
                  _     -> error "Invalid type argued"

-- | Generates a correct expression of type TBool
genBoolExp :: (QCT.MonadGen m, MonadState GlobalContext m) => m Exp
genBoolExp = QCT.sized genBoolExp'

genBoolExp' :: (QCT.MonadGen m, MonadState GlobalContext m) => Int -> m Exp
genBoolExp' 0 = do
  ids   <- inScopeVarsWithType TBool
  QCT.liftGen $ oneof $ [CBool <$> arbitrary] ++ if (length ids > 0)
                                                 then [elements $ Id <$> ids]
                                                 else []
genBoolExp' n | n > 0 = do
  curr <- currentFunctionContext
  let currSub = currSubCtxt curr
      canCall = (Map.size $ tysToFuns currSub) > 0
  QCT.oneof $ [genBoolExp' 0                            ,
               liftM2 Uop genBoolUnop boolExp           ,
               liftM3 Bop genBoolBinop boolExp boolExp  ,
               liftM3 Bop genOrdCompBinop intExp intExp ,
               liftM3 Bop genEqCompBinop intExp intExp  ,
               genCallExpWithType TBool                 ]
    where boolExp = noLoc <$> (genBoolExp' n')
          intExp  = noLoc <$> (genIntExp'  n')
          n'      = n `div` 5 -- 2 and 3 produce stack overflows, went with 5 to be safe

-- | Generates a correct expression of type TInt
genIntExp :: (QCT.MonadGen m, MonadState GlobalContext m) => m Exp
genIntExp = QCT.sized genIntExp'

genIntExp' :: (QCT.MonadGen m, MonadState GlobalContext m) => Int -> m Exp
genIntExp' 0 = do
  ids <- inScopeVarsWithType TInt
  QCT.liftGen $ oneof $ [CInt <$> arbitrary] ++ if (length ids > 0)
                                                then [elements $ Id <$> ids]
                                                else []
genIntExp' n | n > 0 =
  QCT.oneof [genIntExp' 0,
             liftM2 Uop genIntUnop subExp        ,
             liftM3 Bop genIntBinop subExp subExp,
             genCallExpWithType TInt             ]
  where subExp = noLoc <$> (genIntExp' (n `div` 2))



-- | Generates a correct Call expression for a function that returns the argued type
genCallExpWithType :: (QCT.MonadGen m, MonadState GlobalContext m) => Ty -> m Exp
genCallExpWithType ty = do
  currFunId <- gets currentFun
  callTys   <- inScopeCallTysWithRetType ty
  -- prevent recursion
  let callTys' = filter (\ct@(CallTy id _ _) -> id /= currFunId) callTys
  -- don't call functions that take other functions as arguments TODO: allow this
      callTys'' = filter (\ct@(CallTy _ tys _) -> foldr notFuncType True tys) callTys'
      numFuns = length callTys''
  if numFuns == 0
  then QCT.resize 0 (genExpWithType ty) -- default to a non-call expression
  else do
    index   <- QCT.liftGen $ choose (0, (length callTys'') - 1)
    let (CallTy id tys rty) = callTys'' !! index
    bools   <- QCT.vectorOf (length tys) (QCT.resize 0 genBoolExp) -- more than 0 causes stack overflow
    ints    <- QCT.vectorOf (length tys) (QCT.resize 0 genIntExp)
    let args = map (getVal bools ints) (zip tys [0..((length tys) - 1)])
    return $ Call (noLoc $ Id id) (noLoc <$> args)
  where
    notFuncType :: Ty -> Bool -> Bool
    notFuncType ty acc = (ty == TInt || ty == TBool) && acc
    getVal :: [Exp] -> [Exp] -> (Ty, Int) -> Exp
    getVal bools ints (ty, i) = case ty of
                                    TBool -> bools !! i
                                    TInt  -> ints !! i
                                    _     -> error "Function types should be filtered out"

-- QCT Type Generators

-- | Generate an arbitrary primitive Ty
genPrimitiveTy :: (QCT.MonadGen m, MonadState GlobalContext m) => m Ty
genPrimitiveTy = QCT.elements [TBool, TInt]

-- Gen Type Generators

-- | Generate an arbitrary return type
genRetty :: Gen Retty
genRetty = RetVal <$> genPrimTy

-- | Generate FunTy that is a good candidate for our context
-- in that any arguments that are functions will take only
-- primitives for their arguments
genFunTy :: Gen FunTy
genFunTy = do
  funId   <- genFreshId'
  numArgs <- choose (0, 5) -- limit to 5 args
  args    <- vectorOf numArgs genArg
  rty     <- genPrimTy
  return $ FunTy funId args rty

-- | Generate an arg that's either a primitive type or
-- a function whoQCT.se arguments are all primitives
genArg :: Gen (Ty, Id)
genArg = do
  ty <- frequency [(1, genSimpleTRef), (4, genPrimTy)]
  id <- genFreshId'
  return $ (ty, id)

-- | Generate a function type Rty whose arguments are all primitives
genSimpleTRef :: Gen Ty
genSimpleTRef = do
  numArgs <- choose (0, 3) -- limit to 3 args
  args    <- vectorOf numArgs genPrimTy
  rty     <- genRetty
  return $ TRef $ RFun args rty

-- | Generate a FunTy whose arguments are all primitives
genSimpleFunTy :: Gen FunTy
genSimpleFunTy = do
  funId   <- genFreshId'
  numArgs <- choose (0, 3) -- limit to 3 args
  args    <- vectorOf numArgs genSimpleArg
  rty     <- genPrimTy
  return $ FunTy funId args rty

-- | Generate an argument of a primitive type
genSimpleArg :: Gen (Ty, Id)
genSimpleArg = do
  ty <- genPrimTy
  id <- genFreshId'
  return $ (ty, id)

-- | Generate a primitive type
genPrimTy :: Gen Ty
genPrimTy = elements [TBool, TInt]

-- Statement/Block Generators

-- | Generate a list of statements, ending with a return statement into
-- the current subcontext
genFuncBodyBlock :: (QCT.MonadGen m, MonadState GlobalContext m) => m ()
genFuncBodyBlock = do
  numStmts <- QCT.choose (1, 15)  -- limit the number of top level statements in a function body
  QCT.vectorOf numStmts (QCT.oneof [genSimpleStmt, genIfStmt, genWhileStmt, genForStmt])
  genRetStmt

-- | Generate an arbitrary return statement based on the current function's
-- return type and push it to the current subcontext
genRetStmt :: (QCT.MonadGen m, MonadState GlobalContext m) => m ()
genRetStmt = do
  curr <- currentFunctionContext
  let funty@(FunTy _ _ rty) = fty curr
  exp  <- genExpWithType rty
  pushStmt $ Ret $ Just $ noLoc exp

-- | Generate non-nesting statement and add it to the current subcontext
genSimpleStmt :: (QCT.MonadGen m, MonadState GlobalContext m) => m ()
genSimpleStmt = QCT.oneof [genAssnStmt, genDeclStmt]

-- | Generate an arbitrary assign statement and push it to the context
-- or default to a declaration if no variables exist in scope
genAssnStmt :: (QCT.MonadGen m, MonadState GlobalContext m) => m ()
genAssnStmt = do
  ty    <- genPrimitiveTy
  exp   <- genExpWithType ty
  vars  <- inScopeVarsWithType ty
  if (length vars) == 0
  then genDeclStmt
  else do
    index <- QCT.liftGen $ choose (0, (length vars) - 1)
    let id = vars !! index
    pushStmt $ Assn (noLoc $ Id id) (noLoc exp)

-- | Generate an arbitrary delcaration statement and push it to the context
genDeclStmt :: (QCT.MonadGen m, MonadState GlobalContext m) => m ()
genDeclStmt = do
  (ty, vdecl@(Vdecl id nexp)) <- genVdeclWithType
  declVar id ty
  pushStmt $ Decl vdecl

-- | Generate an arbitrary variable delcaration with the type of its expression
genVdeclWithType :: (QCT.MonadGen m, MonadState GlobalContext m) => m (Ty, Vdecl)
genVdeclWithType = do
  id  <- genFreshId
  ty  <- genPrimitiveTy
  exp <- genExpWithType ty
  return $ (ty, Vdecl id (noLoc exp))

-- | Generate an arbitrary if statement and push it to the context
genIfStmt :: (QCT.MonadGen m, MonadState GlobalContext m) => m ()
genIfStmt = do
  condition      <- genBoolExp
  pushSubContext
  numStmts       <- QCT.choose (1, 15)
  QCT.vectorOf numStmts genSimpleStmt
  ifBlock        <- popSubContext
  elseMaybeBlock <- genMaybeBlock
  pushStmt $ If (noLoc condition) ifBlock elseMaybeBlock

-- | Generate a maybe block - an arbitrary choice between Nothing and
-- Just some arbitrary block
genMaybeBlock :: (QCT.MonadGen m, MonadState GlobalContext m) => m (Maybe Block)
genMaybeBlock = do
  pushSubContext -- TODO: refactor so we only do the work if necessary 
  numStmts       <- QCT.choose (1, 15)
  QCT.vectorOf numStmts genSimpleStmt
  block <- popSubContext
  maybe <- QCT.arbitrary'
  if maybe
  then return $ Just $ block
  else return $ Nothing

-- | Generate a while statment that's guaranteed to terminate
-- Method: declare a new integer, but don't add it to the context
-- Therefore, nothing else will change it. At the end of the
-- generated block, add a statement to increment its value
-- so that it approaches the termination condition.
genWhileStmt :: (QCT.MonadGen m, MonadState GlobalContext m) => m ()
genWhileStmt = do
  numIters <- QCT.choose (1, 20) -- perform 1 - 20 iterations
  id       <- genFreshId
  pushStmt $ Decl $ Vdecl id (noLoc $ CInt 0) -- did not add this variable to ctxt
  pushSubContext
  numStmts       <- QCT.choose (1, 15)
  QCT.vectorOf numStmts genSimpleStmt
  block <- popSubContext
  let var  = noLoc $ Id id
      cond = Bop Lt var (noLoc $ CInt numIters)
      assn :: Block
      assn = [noLoc $ Assn var (noLoc $ Bop Add var (noLoc $ CInt 1))]
  pushStmt $ While (noLoc cond) (assn ++ block)

-- | Generate a for loop that's guaranteed to terminate
-- Method: don't add the initialized vars to context so
-- nothing else changes it and increment at every iteration
genForStmt :: (QCT.MonadGen m, MonadState GlobalContext m) => m ()
genForStmt = do
  numIters <- QCT.choose (1, 20) -- perform 1 - 20 iterations
  id       <- genFreshId
  pushSubContext
  numStmts       <- QCT.choose (1, 15)
  QCT.vectorOf numStmts genSimpleStmt
  block <- popSubContext
  let decls  = [Vdecl id (noLoc $ CInt 0)] -- did not add this variable to ctxt
      var    = noLoc $ Id id
      cond   = Bop Lt var (noLoc $ CInt numIters)
      update = Assn var (noLoc $ Bop Add var (noLoc $ CInt 1))
  pushStmt $ For decls (Just $ noLoc cond) (Just $ noLoc update) block

-- Top Level Generators

-- | Generate a function declaration corresponding to the argued FunTy
buildCtxtForFunId :: (QCT.MonadGen m, MonadState GlobalContext m) => Id -> m ()
buildCtxtForFunId id = do
  setCurrentFun id
  genFuncBodyBlock

-- | Generate a program given that the state contains some initial global context
genProg :: StateT GlobalContext Gen ()
genProg = do
  fCtxts <- gets contexts
  let ids = Map.keys fCtxts
  foldr (\id acc -> buildCtxtForFunId id >> acc) (return ()) (ids)

-- Helpers

-- | QCT Monad: Generate a fresh arbitrary variable name
genFreshId :: (QCT.MonadGen m, MonadState GlobalContext m) => m Id
genFreshId = do
  id <- getNextInt
  return $ "var_" ++ (show id)

-- | Gen Monad: Generate a fresh arbitrary variable name
genFreshId' :: Gen Id
genFreshId' = unpack <$> (matchRegexp $ pack "[a-z]\\w+")

