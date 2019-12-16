{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -fwarn-incomplete-patterns #-}

module Interpreter where

import Ast

import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Data.Bits
import qualified Data.Map as Map
import Control.Applicative
import Control.Monad (liftM, liftM2, when)
import Control.Monad.State
import Control.Monad.Except

data ValueTy =
    VBool Bool
  | VInt Int
  | VFun Id
  deriving (Show, Eq)

data GlobalContext
  = GlobalContext {
    currCtxt :: FunctionContext
  , fdecls :: Map.Map Id (Node Fdecl)
  } deriving Show

data FunctionContext
  = FunctionContext {
    -- Decl for this function
    fdecl :: Node Fdecl
    -- Parent context
  , parentCtxt :: Maybe FunctionContext
    -- Subcontext stack
  , currSubCtxt :: FunctionSubContext
    -- Return value
  , retVal :: Maybe ValueTy
  } deriving Show

data FunctionSubContext
  = FunctionSubContext {
    -- Parent context
    parentSubCtxt :: Maybe FunctionSubContext
    -- Bindings for this context
  , vs :: Map.Map Id ValueTy
  } deriving Show

newtype Interp a =
  Interp { runInterp :: ExceptT (Int, String) (State GlobalContext) a }
  deriving (Functor, Applicative, Monad, MonadState GlobalContext,
            MonadError (Int, String))

-- Convienience Initializers

-- | Intializes an empty function sub context with no parent
emptyFunctionSubContext :: FunctionSubContext
emptyFunctionSubContext = FunctionSubContext Nothing Map.empty

-- | Intializes an empty function context with no parent
emptyFunctionContext :: Node Fdecl -> FunctionContext
emptyFunctionContext fdecl =
  FunctionContext fdecl Nothing emptyFunctionSubContext Nothing

-- | Intializes an empty global context
emptyGlobalContext :: Node Fdecl -> GlobalContext
emptyGlobalContext entry = GlobalContext (emptyFunctionContext entry) Map.empty

-- | Initializes context with global decls and sets functions
gCtxtFromProg :: Prog -> Node Fdecl -> Either (Int, String) GlobalContext
gCtxtFromProg (Prog prog) entry =
  (\fs -> (emptyGlobalContext entry) { fdecls = Map.fromList fs }) <$>
    unwrap prog
  where
    unwrap :: [Decl] -> Either (Int, String) [(Id, Node Fdecl)]
    unwrap [] = Right []
    unwrap (Gfdecl f@(Node (Fdecl _ fname _ _) _) : xs) =
      ((fname, f):) <$> unwrap xs
    unwrap _ = Left (12, "Externals are not supported in the interpreter")

-- Helper functions

-- | Looks for a function declaration with the given name
lookUpFdecl :: Id -> Interp (Node Fdecl)
lookUpFdecl x = do
  fs <- gets fdecls
  case Map.lookup x fs of
    Just f  -> return f
    Nothing -> throwError (8, "Function " ++ x ++ " not found")

-- | Looks recursively up the context stack and returns closest found value
lookUpValue :: Id -> Interp ValueTy
lookUpValue x = do
  curr <- gets currCtxt
  lookUpFunSubCtxt x (currSubCtxt curr)
  where
    lookUpFunSubCtxt :: Id -> FunctionSubContext -> Interp ValueTy
    lookUpFunSubCtxt x c =
      case Map.lookup x $ vs c of
        Just v  -> return v
        Nothing -> case parentSubCtxt c of
          Just c' -> lookUpFunSubCtxt x c'
          Nothing -> do
            Node (Fdecl _ fn _ _) _ <- lookUpFdecl x
            return $ VFun fn
            -- TODO better errors, currently will state function not found
            -- throwError (0, "Variable " ++ x ++ " not found")

-- | Looks recursively up the context stack and updates closest value
assignValue :: Id -> ValueTy -> Interp ()
assignValue x v = do
  curr <- gets currCtxt
  currSub' <- setInFunSubCtxt x v (currSubCtxt curr)
  modify $ \s -> s { currCtxt = curr { currSubCtxt = currSub' } }
  where
    setInFunSubCtxt :: Id -> ValueTy -> FunctionSubContext ->
                       Interp FunctionSubContext
    setInFunSubCtxt x v c =
      if Map.member x (vs c) then return c { vs = Map.insert x v $ vs c }
      else case parentSubCtxt c of
        Just c' -> do
          c'' <- setInFunSubCtxt x v c'
          return $ c { parentSubCtxt = Just c'' }
        Nothing -> throwError (0, "Variable " ++ x ++ " not found")

-- | Adds a new binding for the given Id in the current context
declValue :: Id -> ValueTy -> Interp ()
declValue x v = do
  curr <- gets currCtxt
  let currSub = currSubCtxt curr
  let curr' = curr { currSubCtxt = currSub {vs = Map.insert x v (vs currSub) } }
  modify $ \s -> s { currCtxt = curr' }

-- | Sets the return value for the current function, verifies the value type
setRetValue :: Maybe ValueTy -> Interp ()
setRetValue v = do
  curr <- gets currCtxt
  -- Check that this value is of the correct type
  case v of
    Just v' -> do
      let Node (Fdecl (RetVal retty) _ _ _) _ = fdecl curr
      verifyValTy v' retty
    _ -> return ()
  let curr' = curr { retVal = v }
  modify $ \s -> s { currCtxt = curr' }

-- | Creates a new context with the given name and the current as its parent
pushContext :: Node Fdecl -> Interp ()
pushContext f = modify $ \s ->
  let currCtxt' = (emptyFunctionContext f) {parentCtxt = Just $ currCtxt s} in
  s { currCtxt = currCtxt' }

  -- | Sets the current context as the first parent context with a different
-- function name
popContext :: Interp (Maybe ValueTy)
popContext = do
  curr <- gets currCtxt
  let rv = retVal curr
  case parentCtxt curr of
    Just curr' -> modify $ \s -> s { currCtxt = curr' }
    Nothing -> throwError (11, "Cannot pop top level context")
  return $ retVal curr

-- | Creates a new context with the current as its parent with the same name
pushSubContext :: Interp ()
pushSubContext = do
  curr <- gets currCtxt
  let currSub = currSubCtxt curr
  let currSub' = emptyFunctionSubContext { parentSubCtxt = Just currSub }
  modify $ \s -> s { currCtxt = curr { currSubCtxt = currSub' } }

-- | Sets the current context as the current parent and ignores the return value
popSubContext :: Interp ()
popSubContext = do
  curr <- gets currCtxt
  let currSub = currSubCtxt curr
  case parentSubCtxt currSub of
    Just currSub' ->
      modify $ \s -> s { currCtxt = curr { currSubCtxt = currSub' } }
    Nothing -> throwError (13, "Cannot pop top level subcontext")

--------------------------------------------------------------------------

-- | Verify that the given values match the specified types
verifyArgTypes :: [ValueTy] -> [Ty] -> Interp ()
verifyArgTypes (t1:xs) (t2:ys) = do
  verifyValTy t1 t2
  verifyArgTypes xs ys
verifyArgTypes [] [] = return ()
verifyArgTypes _ _ = throwError (9, "Function call type mismatch")

-- | Verify that the given value matches the given type
verifyValTy (VInt _) TInt = return ()
verifyValTy (VBool _) TBool = return ()
verifyValTy (VFun id) (TRef (RFun tys1 retty1)) = do
  Node (Fdecl retty2 _ tyargs _) _ <- lookUpFdecl id
  when (retty1 /= retty2 || tys1 /= map fst tyargs) $
    throwError (9, "Function type mismatch")
verifyValTy _ _ = throwError (9, "Type mismatch")

-- | Error to be thrown to exit out of function due to an early return statement
returnError :: (Int, String)
returnError = (-1, "Return")

-- | Evaluate an expression
evalE :: Node Exp -> Interp ValueTy
evalE (Node (CBool b) _) = return $ VBool b
evalE (Node (CInt i) _) = return $ VInt i
evalE (Node (Id x) _) = lookUpValue x
evalE (Node (Call ef args) _) = evalCall ef args returnHandler
  where
    returnHandler :: Maybe ValueTy -> Interp ValueTy
    returnHandler (Just v) = return v
    returnHandler Nothing =
      throwError (10, "Cannot call void function as an expression")
evalE (Node (Bop o e1 e2) loc) = do
  e1' <- evalE e1
  e2' <- evalE e2
  evalBop o e1' e2' loc
evalE (Node (Uop o e) loc) = do
  e' <- evalE e
  evalUop o e' loc

-- | Evaluate a binary op
evalBop :: Binop -> ValueTy -> ValueTy -> Loc -> Interp ValueTy
evalBop Add  (VInt i1) (VInt i2) _ = return $ VInt (i1 + i2)
evalBop Sub  (VInt i1) (VInt i2) _ = return $ VInt (i1 - i2)
evalBop Mul  (VInt i1) (VInt i2) _ = return $ VInt (i1 * i2)
evalBop Div  (VInt i1) (VInt  0) _ = throwError (1, "Divide by zero")
evalBop Div  (VInt i1) (VInt i2) _ = return $ VInt (i1 `div` i2)
evalBop IAnd (VInt i1) (VInt i2) _ = return $ VInt (i1 .&. i2)
evalBop IOr  (VInt i1) (VInt i2) _ = return $ VInt (i1 .|. i2)
evalBop Shl  (VInt i1) (VInt i2) _ = return $ VInt (i1 `shiftL` i2)
evalBop Shr  (VInt i1) (VInt i2) _ = return $ VInt (i1 `shiftR` i2)
evalBop Mod  (VInt i1) (VInt i2) _ = return $ VInt (i1 `mod` i2)
evalBop Eq   (VInt i1) (VInt i2) _ = return $ VBool (i1 == i2)
evalBop Neq  (VInt i1) (VInt i2) _ = return $ VBool (i1 /= i2)
evalBop Lt   (VInt i1) (VInt i2) _ = return $ VBool (i1 < i2)
evalBop Lte  (VInt i1) (VInt i2) _ = return $ VBool (i1 <= i2)
evalBop Gt   (VInt i1) (VInt i2) _ = return $ VBool (i1 > i2)
evalBop Gte  (VInt i1) (VInt i2) _ = return $ VBool (i1 >= i2)
evalBop And  (VBool i1) (VBool i2) _ = return $ VBool (i1 && i2)
evalBop Or   (VBool i1) (VBool i2) _ = return $ VBool (i1 || i2)
evalBop o _ _ _ = throwError (2, "Invalid types for op " ++ show o)

-- | Evaluate a unary op
evalUop :: Unop -> ValueTy -> Loc -> Interp ValueTy
evalUop Neg (VInt i) _ = return $ VInt (-i)
evalUop Lognot (VBool i) _ = return $ VBool (not i)
evalUop o _ _ = throwError (2, "Invalid types for unop " ++ show o)

-- | Evaluate a statement
evalS :: Node Stmt -> Interp ()
evalS (Node (Assn (Node (Id x) _) e) _) = do
  -- TODO evalId? we may need to evaluate x to find the loc to store e
  v <- evalE e
  assignValue x v
evalS (Node (Assn _ _) _) = throwError (4, "Must assign to a variable")
evalS (Node (Decl (Vdecl x e)) _) = do
  v <- evalE e
  declValue x v
evalS (Node (Ret (Just e)) _) = do
  v <- evalE e
  setRetValue $ Just v
  throwError returnError
evalS (Node (Ret Nothing) _) = do
  setRetValue Nothing
  throwError returnError
evalS (Node (SCall ef args) _) = void $ evalCall ef args return
evalS (Node (If e b1 b2) _) = do
  v <- evalE e
  pushSubContext
  case v of
    VBool True  -> evalB b1
    VBool False -> evalB $ fromMaybe [noLoc Nop] b2
    _           -> throwError (5, "If condition must eval to bool")
  popSubContext
evalS (Node (For vs cond iter ss) loc) = do
  forM_ vs $ \(Vdecl x e) -> do
    e' <- evalE e
    declValue x e'
  let cond' = fromMaybe (Node (CBool True) loc) cond
  let iter' = fromMaybe (Node Nop loc) iter
  evalS $ Node (While cond' (ss ++ [iter'])) loc
evalS w@(Node (While e ss) _) = do
  v <- evalE e
  pushSubContext
  case v of
    VBool True  -> evalB (ss ++ [w])
    VBool False -> return ()
    _           -> throwError (3, "Loop condition must eval to bool")
  popSubContext
evalS (Node Nop _) = return ()

-- | Evaluate a call, used for both expression calls and statement calls.
-- Passes the optional return value to the handler and returns the handler
-- result.
evalCall :: forall a. Ast.Node Ast.Exp -> [Ast.Node Ast.Exp] ->
            (Maybe ValueTy -> Interp a) -> Interp a
evalCall ef args returnHandler = do
  fv <- evalE ef
  case fv of
    VFun id -> do
      nf@(Node (Fdecl _ _ tyargs body) _) <- lookUpFdecl id
      argvals <- mapM evalE args
      verifyArgTypes argvals (Prelude.map fst tyargs)
      let ids = map snd tyargs
      pushContext nf
      mapM_ (uncurry declValue) (zip ids argvals)
      catchError (do { evalB body ; returnErrorHandler returnError }) returnErrorHandler
    _ -> throwError (3, "Cannot call a non-function pointer")
  where
    returnErrorHandler :: (Int, String) -> Interp a
    returnErrorHandler (-1, _) = popContext >>= returnHandler
    returnErrorHandler e = throwError e

-- | Evaluate a block
evalB :: Block -> Interp ()
evalB = mapM_ evalS

runBlock :: Block ->
            GlobalContext ->
            (Either (Int, String) (), GlobalContext)
runBlock = runState . runExceptT . runInterp . evalB

executeProg :: Prog -> Id -> Either (Int, String) ValueTy
executeProg p@(Prog prog) entry =
  let entryF = find (\g -> nameFromDecl g == entry) prog in
  case entryF of
    Just (Gfdecl nf@(Node (Fdecl _ _ [] b) _)) ->
      let gCtxt = gCtxtFromProg p nf in
      case gCtxt of
        Right startCtxt ->
          let (res, finalCtxt) = runBlock b startCtxt in
          case (res, retVal $ currCtxt finalCtxt) of
            (Left (-1, _), Just r) -> Right r
            (Left e, _) -> Left e
            (Right _, Just r) -> Right r
            (Right _, Nothing) -> Left (10, "Function did not return a value")
        Left e -> Left e
    Just _ -> Left (6, "Entry function must not take in any arguments")
    Nothing -> Left (7, "Could not find entry function")


runInterpreter :: Prog -> Id -> IO ()
runInterpreter prog entry = do
  let r = executeProg prog entry
  putStrLn (display r)
  where
    -- Display either the error or resulting value
    display :: Either (Int, String) ValueTy -> String
    display (Left (_, v))  = "Exception: " ++ v
    display (Right v) = "Result: " ++ showVType v
    -- Display the underlying value in the value type
    showVType :: ValueTy -> String
    showVType (VInt i) = show i
    showVType (VBool True) = "true"
    showVType (VBool False) = "false"
    showVType (VFun f) = f
