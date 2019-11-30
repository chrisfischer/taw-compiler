{-# LANGUAGE FlexibleContexts #-}

module Interpreter where

import Ast

import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Data.Bits
import qualified Data.Map as Map
import Control.Applicative
import Control.Monad (liftM, liftM2)
import Control.Monad.State
import Control.Monad.Except (MonadError(..), ExceptT, runExceptT)

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
emptyFunctionContext fn = FunctionContext fn Nothing Map.empty Nothing

-- | Intializes an empty global context with no parent
emptyGlobalContext entry = GlobalContext (emptyFunctionContext entry) Map.empty

-- | Initializes context with global decls and sets functions
gCtxtFromProg :: Id -> Prog -> Either (Int, String) GlobalContext
gCtxtFromProg entry prog =
  fmap (\fs -> (emptyGlobalContext entry) { fdecls = Map.fromList fs }) $
    unwrap prog
  where
    unwrap :: Prog -> Either (Int, String) [(Id, Node Fdecl)]
    unwrap [] = Right []
    unwrap ((Gfdecl f@(Node (Fdecl _ fname _ _) _)):xs) =
      fmap ((fname, f):) $ unwrap xs
    unwrap _ = Left (12, "Externals are not supported in the interpreter")

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
  lookUpFunCtxt x curr (fnm curr) where
    lookUpFunCtxt :: (MonadError (Int, String) m, MonadState GlobalContext m) =>
                     Id -> FunctionContext -> Id -> m ValueTy
    lookUpFunCtxt x c fid =
      case Map.lookup x $ vs c of
        Just v  -> return v
        Nothing -> case parentCtxt c of
          Just c' | (fnm c' == fid) -> lookUpFunCtxt x c' fid
          _ -> do
            Node (Fdecl _ fn _ _) _ <- lookUpFdecl x
            return $ VFun fn
            -- TODO better errors
            -- throwError (0, "Variable " ++ x ++ " not found")

-- | Looks recursively up the context stack and updates closest value
assignValue :: (MonadError (Int, String) m, MonadState GlobalContext m) =>
            Id -> ValueTy -> m ()
assignValue x v = do
  curr <- gets currCtxt
  setInFunCtxt x v curr (fnm curr) where
    setInFunCtxt :: (MonadError (Int, String) m, MonadState GlobalContext m)
                    => Id -> ValueTy -> FunctionContext -> Id -> m ()
    setInFunCtxt x v c fid =
      case Map.lookup x $ vs c of
        Just v -> modify $ \s ->
          s { currCtxt = (currCtxt s) { vs = Map.insert x v $ vs c } }
        Nothing -> case parentCtxt c of
          Just c' | (fnm c' == fid) -> setInFunCtxt x v c' fid
          _ -> throwError (0, "Variable " ++ x ++ " not found")

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
  -- Node (Fdecl (RetVal retty) _ _ _) _ <- lookUpFdecl (fnm curr)
  -- verifyRetTy v retty
  let curr' = curr { retVal = Just v }
  modify $ \s -> s { currCtxt = curr' }
  where
    verifyRetTy :: (MonadError (Int, String) m) => ValueTy -> Ty -> m ()
    verifyRetTy (VInt _) TInt = return ()
    verifyRetTy (VBool _) TBool = return ()
    verifyRetTy (VFun _) (TRef (RFun _ _)) = return ()
    verifyRetTy _ _ = throwError (9, "Ret type mismatch")

-- | Creates a new context with the given name and the current as its parent
pushContext :: MonadState GlobalContext m => Id -> m ()
pushContext name = modify $ \s ->
  s { currCtxt = (emptyFunctionContext name) {
      parentCtxt = (Just $ currCtxt s) } }

-- | Creates a new context with the current as its parent with the same name
pushSubContext :: MonadState GlobalContext m => m ()
pushSubContext = modify $ \s ->
  s { currCtxt = (emptyFunctionContext $ fnm (currCtxt s)) {
      parentCtxt = (Just $ currCtxt s) } }

-- | Sets the current context as the current parent and ignores the return value
popSubContext :: MonadState GlobalContext m => m ()
popSubContext = do
  curr <- gets currCtxt
  let curr' = case parentCtxt curr of
                Nothing -> curr
                Just c  -> c
  modify $ \s -> s { currCtxt = curr' }

-- | Sets the current context as the first parent context with a different
-- function name
popContext :: (MonadError (Int, String) m, MonadState GlobalContext m) =>
              m (ValueTy)
popContext = do
  curr <- gets currCtxt
  let f = fnm curr
  case retVal curr of
    Just rv -> case findDifferentParent f curr of
      Just c' -> do
        modify $ \s -> s { currCtxt = c' }
        return rv
      Nothing -> throwError (11, "Cannot pop top level context")
    Nothing -> throwError (10, "Function did not return a value")
  where
    findDifferentParent :: Id -> FunctionContext -> Maybe FunctionContext
    findDifferentParent id c =
      if fnm c == id then do
        p <- parentCtxt c
        p' <- findDifferentParent id p
        return p'
      else Just c

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
      forM_ (zip ids argvals) $ \(id, v) -> declValue id v
      evalB body
      popContext
    _ -> throwError (3, "Cannot call a non-function pointer")
  where
    -- | Verify that the given types match the specified types
    verifyArgTypes :: (MonadError (Int, String) m) => [ValueTy] -> [Ty] -> m ()
    verifyArgTypes ((VInt _):xs) (TInt:ys) = verifyArgTypes xs ys
    verifyArgTypes ((VBool _):xs) (TBool:ys) = verifyArgTypes xs ys
    verifyArgTypes ((VFun _):xs) ((TRef (RFun _ _)):ys) = verifyArgTypes xs ys
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
  popSubContext
evalS (Node (For vs cond iter ss) loc) = do
  pushSubContext
  forM_ vs $ \(Vdecl x e) -> do
    e' <- evalE e
    assignValue x e'
  let cond' = fromMaybe (Node (CBool True) loc) cond
  let iter' = fromMaybe (Node (Nop) loc) iter
  evalS $ Node (While cond' (iter' : ss)) loc
  popSubContext
evalS w@(Node (While e ss) _) = do
  v <- evalE e
  pushSubContext
  case v of
    VBool True  -> evalB (ss ++ [w])
    VBool False -> return ()
    VInt  _     -> throwError (3, "Loop condition must eval to bool")
  popSubContext
evalS (Node Nop _) = return ()

-- | Evaluate a block
evalB :: (MonadError (Int, String) m, MonadState GlobalContext m) =>
         Block -> m ()
evalB = mapM_ evalS

executeBlock :: Block ->
           GlobalContext ->
           (Either (Int, String) (), GlobalContext)
executeBlock b gCtxt = runState (runExceptT $ evalB b) gCtxt

executeProg :: Id -> Prog -> Either (Int, String) ValueTy
executeProg entry prog =
  let entryF = find (\(Gfdecl (Node (Fdecl _ fname _ _) _)) ->
                        fname == entry) prog
      gCtxt = gCtxtFromProg entry prog in
  case gCtxt of
    Right startCtxt ->
      case entryF of
        Just (Gfdecl (Node (Fdecl _ _ [] b) _)) ->
          let (res, finalCtxt) = executeBlock b startCtxt in
          case (res, retVal $ currCtxt $ finalCtxt) of
            (Left e, _) -> Left e
            (Right _, Just r) -> Right r
            (Right _, Nothing) -> Left (10, "Function did not return a value")
        Just _ -> Left (6, "Entry function must not take in any arguments")
        Nothing -> Left (7, "Could not find entry function")
    Left e -> Left e


-- idd x = noLoc $ Id x

-- testFDecl2 = Fdecl (RetVal TInt) "add" []
--   [ noLoc $ Decl $ Vdecl "x" $ noLoc $ CInt 42
--   , noLoc $ Decl $ Vdecl "y" $ noLoc $ CInt 10
--   , noLoc $ Decl $ Vdecl "z" $ noLoc $ Bop Add (idd "x") (idd "y")
--   , noLoc $ Ret $ noLoc (Id "z") ]

-- testFDecl = Fdecl (RetVal TInt) "main" []
--   [ noLoc $ Decl $ Vdecl "x" $ noLoc $ CInt 42
--   , noLoc $ Decl $ Vdecl "y" $ noLoc $ Call (idd "add") []
--   , noLoc $ Decl $ Vdecl "z" $ noLoc $ Bop Add (idd "x") (idd "y")
--   , noLoc $ Ret $ noLoc (Id "z") ]

-- testProg = [
--     Gfdecl $ noLoc testFDecl2
--   , Gfdecl $ noLoc testFDecl ]

idd x = noLoc $ Id x

testFDecl2 = Fdecl (RetVal TInt) "add" [(TInt, "x"), (TInt, "y")]
  [ noLoc $ Decl $ Vdecl "z" $ noLoc $ Bop Add (idd "x") (idd "y")
  , noLoc $ Ret $ noLoc (Id "z") ]

testFDecl3 = Fdecl (RetVal TInt) "do" [(TRef (RFun [TInt, TInt] (RetVal TInt)), "f1"), (TInt, "x"), (TInt, "y")]
  [ noLoc $ Decl $ Vdecl "z" $ noLoc $ Call (idd "f1") [(idd "x"), (idd "y")]
  , noLoc $ Ret $ noLoc (Id "z") ]

testFDecl = Fdecl (RetVal TInt) "main" []
  [ noLoc $ Decl $ Vdecl "x" $ noLoc $ CInt 42
  , noLoc $ Decl $ Vdecl "f" $ noLoc $ Id "add"
  , noLoc $ Decl $ Vdecl "y" $ noLoc $ Call (idd "do") [(idd "f"), (idd "x"), (idd "x")]
  , noLoc $ Decl $ Vdecl "z" $ noLoc $ Bop Add (idd "x") (idd "y")
  , noLoc $ Ret $ idd "z" ]

testProg = [
    Gfdecl $ noLoc testFDecl2
  , Gfdecl $ noLoc testFDecl3
  , Gfdecl $ noLoc testFDecl ]


run :: Id -> Prog -> IO ()
run entry prog = do
  let r = executeProg entry prog
  putStrLn (display r)

display :: Show a => (Either (Int, String) a) -> String
display (Left (_, v))  = "Exception: " ++ v
display (Right v) = "Result: " ++ show v
