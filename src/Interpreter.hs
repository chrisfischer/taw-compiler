{-# LANGUAGE FlexibleContexts #-}

import Ast -- as Taw

import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Data.Bits
import qualified Data.Map as Map
import Control.Applicative
import Control.Monad (liftM, liftM2)
import Control.Monad.State
import Control.Monad.Except (MonadError(..), ExceptT, runExceptT)


data GlobalContext
  = GlobalContext {
    currCtxt :: FunctionContext
  , fdecls :: Map.Map Id (Node Fdecl)
  } deriving Show

data FunctionContext
  = FunctionContext {
    -- Name of function
    fnm :: Id,
    -- Parent context
    parentCtxt :: Maybe FunctionContext
    -- Bindings for this context
  , vs :: Map.Map Id ValueTy
  , retVal :: Maybe ValueTy
  } deriving Show

-- Convienience Initializers

-- | Intializes an empty function context with no parent
emptyFunctionContext = FunctionContext "<string>" Nothing Map.empty Nothing

-- | Intializes an empty global context with no parent
emptyGlobalContext = GlobalContext emptyFunctionContext Map.empty

-- | Initializes context with global decls and sets functions
gCtxtFromProg :: Prog -> GlobalContext
gCtxtFromProg prog =
  let fs = map (\(Gfdecl f@(Node (Fdecl _ fname _ _) _)) -> (fname, f)) prog in
  emptyGlobalContext { fdecls = Map.fromList fs }

-- Helper functions

lookUpFdecl :: (MonadError (Int, String) m, MonadState GlobalContext m) =>
               Id -> m (Node Fdecl)
lookUpFdecl x = do
  fs <- gets fdecls
  case Map.lookup x fs of
    Just f  -> return f
    Nothing -> throwError (8, "Function " ++ x ++ " not found")

-- | Looks recursively up the context stack and returns closest found value
lookUpValue :: (MonadError (Int, String) m, MonadState GlobalContext m) =>
               Id -> m ValueTy
lookUpValue x = do
  curr <- gets currCtxt
  lookUpFunCtxt x curr where
    lookUpFunCtxt :: (MonadError (Int, String) m, MonadState GlobalContext m) =>
                     Id -> FunctionContext -> m ValueTy
    lookUpFunCtxt x c =
      case Map.lookup x $ vs c of
        Just v  -> return v
        Nothing -> case parentCtxt c of
          Just c' -> lookUpFunCtxt x c'
          Nothing -> do
            Node (Fdecl _ fn _ _) _ <- lookUpFdecl x
            return $ VFun fn
            -- TODO better errors
            -- throwError (0, "Variable " ++ x ++ " not found")

-- | Looks recursively up the context stack and updates closest value
assignValue :: (MonadError (Int, String) m, MonadState GlobalContext m) =>
            Id -> ValueTy -> m ()
assignValue x v = do
  curr <- gets currCtxt
  setInFunCtxt x v curr where
    setInFunCtxt :: (MonadError (Int, String) m, MonadState GlobalContext m)
                    => Id -> ValueTy -> FunctionContext -> m ()
    setInFunCtxt x v c =
      case Map.lookup x $ vs c of
        Just v -> modify $ \s ->
          s { currCtxt = (currCtxt s) { vs = Map.insert x v $ vs c } }
        Nothing -> case parentCtxt c of
          Just c' -> setInFunCtxt x v c'
          Nothing -> throwError (0, "Variable " ++ x ++ " not found")

-- | Adds a new binding for the given Id in the current context
declValue :: MonadState GlobalContext m => Id -> ValueTy -> m ()
declValue x v = do
  curr <- gets currCtxt
  let curr' = curr { vs = Map.insert x v (vs curr) }
  modify $ \s -> s { currCtxt = curr' }

setRetValue :: (MonadError (Int, String) m, MonadState GlobalContext m) =>
               ValueTy -> m ()
setRetValue v = do
  curr <- gets currCtxt
  Node (Fdecl (RetVal retty) _ _ _) _ <- lookUpFdecl (fnm curr)
  validateRetTy v retty
  let curr' = curr { retVal = Just v }
  modify $ \s -> s { currCtxt = curr' }
  where
    validateRetTy :: (MonadError (Int, String) m) => ValueTy -> Ty -> m ()
    validateRetTy (VInt _) TInt = return ()
    validateRetTy (VBool _) TBool = return ()
    validateRetTy _ _ = throwError (9, "Ret type mismatch")

-- | Creates a new context with the given name and the current as its parent
pushContext :: MonadState GlobalContext m => Id -> m ()
pushContext name = modify $ \s ->
  s { currCtxt = emptyFunctionContext {
      fnm = name
    , parentCtxt = (Just $ currCtxt s) } }

-- | Creates a new context with the current as its parent with the same name
pushSubContext :: MonadState GlobalContext m => m ()
pushSubContext = modify $ \s ->
  s { currCtxt = emptyFunctionContext {
      fnm = fnm (currCtxt s)
    , parentCtxt = (Just $ currCtxt s) } }

-- TODO pop contexts until we reach the last function
-- | Sets the current context as the current parent and returns an optional
-- return value
popContext :: MonadState GlobalContext m => m (Maybe ValueTy)
popContext = do
  curr <- gets currCtxt
  let curr' = case parentCtxt curr of
                Nothing -> curr
                Just c  -> c
  modify $ \s -> s { currCtxt = curr' }
  return $ retVal curr

--------------------------------------------------------------------------

-- | Evaluate an expression
evalE :: (MonadError (Int, String) m, MonadState GlobalContext m) =>
         Node Exp -> m ValueTy
evalE (Node (CBool b) _) = return $ VBool b
evalE (Node (CInt i) _) = return $ VInt i
evalE (Node (Id x) _) = do
  v <- lookUpValue x
  return v
evalE (Node (Call ef args) _) = do
  fv <- evalE ef
  case fv of
    VFun id -> do
      Node (Fdecl _ _ tyargs body) _ <- lookUpFdecl id
      argvals <- mapM evalE args
      verifyArgTypes argvals (Prelude.map fst tyargs)
      let ids = map snd tyargs
      pushContext id
      mapM_ (\(id, v) -> assignValue id v) (zip ids argvals)
      evalB body
      retVal <- popContext
      case retVal of
        Just rv -> return rv
        Nothing -> throwError (10, "Function did not return a value")
    _ -> throwError (3, "Cannot call a non-function pointer")
  where
    -- | Verify that the given types match the specified types
    verifyArgTypes :: (MonadError (Int, String) m) => [ValueTy] -> [Ty] -> m ()
    verifyArgTypes ((VInt _):xs) (TInt:ys) = verifyArgTypes xs ys
    verifyArgTypes [] [] = return ()
    verifyArgTypes _ _ = throwError (9, "Function call type mismatch")

evalE (Node (Bop o e1 e2) loc) = do
  e1' <- evalE e1
  e2' <- evalE e2
  evalBop o e1' e2' loc
evalE (Node (Uop o e) loc) = do
  e' <- evalE e
  evalUop o e' loc

-- | Evaluate a binary op
evalBop :: MonadError (Int, String) m =>
          Binop -> ValueTy -> ValueTy -> Loc -> m ValueTy
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
evalUop :: MonadError (Int, String) m => Unop -> ValueTy -> Loc -> m ValueTy
evalUop Neg (VInt i) _ = return $ VInt (-i)
evalUop Lognot (VBool i) _ = return $ VBool (not i)

-- | Evaluate a statement
evalS :: (MonadError (Int, String) m, MonadState GlobalContext m) =>
         Node Stmt -> m ()
evalS (Node (Assn (Node (Id x) _) e) _) = do
  -- TODO evalId? we may need to evaluate x to find the loc to store e
  v <- evalE e
  assignValue x v
evalS (Node (Assn _ _) _) = throwError (4, "Must assign to a variable")
evalS (Node (Decl (Vdecl x e)) _) = do
  v <- evalE e
  declValue x v
evalS (Node (Ret e) _) = do
    v <- evalE e
    setRetValue v
evalS (Node (If e b1 b2) _) = do
  v <- evalE e
  pushSubContext
  case v of
    VBool True  -> evalB b1
    VBool False -> evalB b2
    VInt  _     -> throwError (5, "If condition must eval to bool")
  _ <- popContext
  return ()
evalS (Node (For vs cond iter ss) loc) = do
  pushSubContext
  mapM_ (\(Vdecl x e) -> do
    e' <- evalE e
    assignValue x e') vs
  let cond' = fromMaybe (Node (CBool True) loc) cond
  let iter' = fromMaybe (Node (Nop) loc) iter
  evalS $ Node (While cond' (iter' : ss)) loc
  _ <- popContext
  return ()
evalS w@(Node (While e ss) _) = do
  v <- evalE e
  pushSubContext
  case v of
    VBool True  -> evalB (ss ++ [w])
    VBool False -> return ()
    VInt  _     -> throwError (3, "Loop condition must eval to bool")
  _ <- popContext
  return ()
evalS (Node Nop _) = return ()

-- | Evaluate a block
evalB :: (MonadError (Int, String) m, MonadState GlobalContext m) =>
         Block -> m ()
evalB = mapM_ evalS

executeBlock :: Block ->
           GlobalContext ->
           (Either (Int, String) (), GlobalContext)
executeBlock b gCtxt = runState (runExceptT $ evalB b) gCtxt

executeProg :: Id -> Prog -> (Either (Int, String) (), GlobalContext)
executeProg entry prog =
  let entryF = find (\(Gfdecl (Node (Fdecl _ fname _ _) _)) -> fname == entry) prog
      gCtxt = gCtxtFromProg prog in
  case entryF of
    Just (Gfdecl (Node (Fdecl _ _ [] b) _)) -> executeBlock b gCtxt
    Just _ -> (Left (6, "Entry function must not take in any arguments"), gCtxt)
    Nothing -> (Left (7, "Could not find entry function"), gCtxt)


idd x = Node (Id x) 0
testFDecl2 = Fdecl (RetVal TInt) "add" []
  [ Node (Decl (Vdecl "x" (Node (CInt 42) 0 ))) 0
  , Node (Decl (Vdecl "y" (Node (CInt 10) 0 ))) 0
  , Node (Decl (Vdecl "z" (Node (Bop Add (idd "x") (idd "y")) 0 ))) 0
  , Node (Ret (Node (Id "z") 0)) 0 ]

testFDecl = Fdecl (RetVal TInt) "main" []
  [ Node (Decl (Vdecl "x" (Node (CInt 42) 0 ))) 0
  , Node (Decl (Vdecl "y" (Node (Call (idd "add") []) 0 ))) 0
  , Node (Decl (Vdecl "z" (Node (Bop Add (idd "x") (idd "y")) 0 ))) 0
  , Node (Ret (Node (Id "z") 0)) 0 ]

testProg = [
    Gfdecl (Node testFDecl2 0)
  , Gfdecl (Node testFDecl 0) ]


run :: Id -> Prog -> IO ()
run entry prog = do
  let (r, s) = executeProg entry prog
  putStrLn (display r)
  putStr "Result: "
  putStrLn (show $ retVal $ currCtxt s)

display :: Show a => (Either (Int, String) a) -> String
display (Left (_, v))  = "Uncaught exception: " ++ v
display (Right v) = "Result: " ++ show v