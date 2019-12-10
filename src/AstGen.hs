{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -fwarn-incomplete-patterns #-}

module AstGen where

import Data.Maybe (fromMaybe)
import Data.Tuple (swap)

import Control.Applicative
import Control.Monad
import Control.Monad.State

import Test.QuickCheck
import Test.QuickCheck.Gen
import qualified QuickCheck.GenT as QCT
import qualified Data.Map as Map

import Ast

--------------------------------------
-- UTILS - ---------------------------
--------------------------------------

nol x = noLoc <$> x

emptyFdecl r fn as = Fdecl r fn as []

-- Name, arg types and names, return type (must not be void)
data FunTy = FunTy Id [(Ty, Id)] Ty deriving Show

--------------------------------------
-- CONTEXT ---------------------------
--------------------------------------

data GlobalContext
  = GlobalContext {
    -- Function currently being generated
    currentFun :: Id
    -- Map from function name to its context
  , contexts :: Map.Map Id FunctionContext
    -- Map from retty to function type
  , funs :: Map.Map Ty FunTy
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
    -- Accumulated statements in this scope
  , stmts :: Block
  } deriving Show


-- newtype AstGenerator a =
  --   AstGenerator { runAstGenerator :: QCT.GenT (State GlobalContext) a }
  --   deriving (Functor, Applicative, Monad, QCT.MonadGen, MonadState GlobalContext)

execAstGenerator :: FunTy -> [FunTy] -> [Fdecl]
execAstGenerator main fs =
  let gCtxt = initGlobalContext main fs
  -- TODO actually implement the monad to generate instead of (return ())
      gCtxt' = execState (runAstGenerator (return ())) gCtxt in
  map fCtxtToFdecl $ Map.elems (contexts gCtxt')
  where
    fCtxtToFdecl :: FunctionContext -> Fdecl
    fCtxtToFdecl (FunctionContext fty (FunctionSubContext Nothing _ _ block)) = funTytoFdecl fty (reverse block)
    fCtxtToFdecl _ = error "Subcontext is not root"
    funTytoFdecl :: FunTy -> Block -> Fdecl
    funTytoFdecl (FunTy fn args retty) b = Fdecl (RetVal retty) fn args b


-- Convienience Initializers

-- | Intializes an empty function sub context with no parent
emptyFunctionSubContext :: FunctionSubContext
emptyFunctionSubContext = FunctionSubContext Nothing Map.empty Map.empty []

-- | Intializes an empty function context with no parent, initalizes declared
-- variables with the arguments
emptyFunctionContext :: FunTy -> FunctionContext
emptyFunctionContext fty@(FunTy _ args _) =
  FunctionContext fty emptyFunctionSubContext {
      vars = Map.fromList $ map swap args
    , tysToVars = foldr (\(ty, id) acc -> Map.alter (Just [id] <>) ty acc) Map.empty args
    }

-- | Intializes an empty global context
initGlobalContext :: FunTy -> [FunTy] -> GlobalContext
initGlobalContext main@(FunTy fn _ _) fs =
  let fs' = main : fs in
  let fds = Map.fromList $ map (\f@(FunTy _ _ retty) -> (retty, f)) fs'
      ctxts = Map.fromList $
        map (\f@(FunTy fn _ _) -> (fn, emptyFunctionContext f)) fs'
  in
  GlobalContext fn ctxts fds


-- Helper functions

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

-- | Looks for a function declaration with the given name
lookUpFunTy :: (QCT.MonadGen m, MonadState GlobalContext m) =>
               Ty -> m (Maybe FunTy)
lookUpFunTy x = do
  fs <- gets funs
  return $ Map.lookup x fs

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
      Map.unionWith (+) pvars (vars c)

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
    mergeMaps :: Map.Map k [a] -> Map.Map k [a] -> Map.Map k [a]
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
  return $ stmts currSub

pushStmt :: (QCT.MonadGen m, MonadState GlobalContext m) => Stmt -> m ()
pushStmt s = do
  curr <- currentFunctionContext
  let currSub = currSubCtxt curr
  let currSub' = currSub { stmts = noLoc s : (stmts currSub) }
  modifyCurrentFunctionContext curr { currSubCtxt = currSub' }

data Ctxt = Ctxt {
--   -- lookup vars by ty string
--   vars :: Map.Map String [String]
--   -- lookup functions by retty string
-- , funs :: Map.Map String [String]
--   -- lookup functions Rtys by id
-- , funTypes :: Map.Map String Rty
-- -- , currentFun :: String
}

-- helpers
idsOfType :: String -> Ctxt -> [Exp]
idsOfType t c =
  case Map.lookup t (vars c) of
    Nothing   -> []
    Just vars -> Ast.Id <$> vars

funsOfType :: String -> Ctxt -> [String]
funsOfType t c =
  case Map.lookup t (funs c) of
    Nothing   -> []
    Just funs -> funs

currentRetty :: Ctxt -> Retty
currentRetty c = let RFun _ retty = funTypes c (Map.!) (currentFun c) in retty

-- test contexts
myC :: Ctxt
myC = Ctxt (Map.insert "TBool" ["bool1", "bool2"]
             (Map.insert "TInt" ["int1", "int2"] Map.empty))
           (Map.insert "TBool" ["fBool"] Map.empty)
           (Map.insert "fBool" (RFun [TBool, TBool] (RetVal TBool)) Map.empty)
           ""

--------------------------------------
-- OPERATOR GENERATORS ---------------
--------------------------------------

genBoolUnop :: Gen Unop
genBoolUnop = return Ast.Neg

genBoolBinop :: Gen Binop
genBoolBinop = elements [Ast.And, Ast.Or]

genIntBinop :: Gen Binop
genIntBinop = elements [Ast.Add, Ast.Sub, Ast.Mul, Ast.Div, Ast.Mod]
-- ^ add logical operators here

genOrdCompBinop :: Gen Binop
genOrdCompBinop = elements [Ast.Lt, Ast.Lte, Ast.Gt, Ast.Gte]

genEqCompBinop :: Gen Binop
genEqCompBinop = elements [Ast.Eq, Ast.Neq]


--------------------------------------
-- EXPRESSION GENERATORS -------------
--------------------------------------
genMaybeNodeExp :: Ctxt -> Gen (Maybe (Node Exp))
genMaybeNodeExp c =
  oneof [Just <$> genNodeExp c,
         return Nothing       ]

genNodeExp :: Ctxt -> Gen (Node Exp)
genNodeExp c = noLoc <$> (genExp c)

genExp :: Ctxt -> Gen Exp
genExp c = sized (genExp' c)

genExp' :: Ctxt -> Int -> Gen Exp
genExp' c n =
  oneof [genIntExp'  c n,
         genBoolExp' c n]

genIntExp :: Ctxt -> Gen Exp
genIntExp c = sized (genIntExp' c)

genIntExp' :: Ctxt -> Int -> Gen Exp
genIntExp' c 0 =
  let arbGen   = CInt <$> arbitrary in
  let idGens   = return <$> (idsOfType "TInt" c) in
  oneof $ [arbGen] ++ idGens -- TODO: fix this so it's evenly balanced
genIntExp' c n | n > 0 =
  oneof [genIntExp' c 0,
         liftM3 Bop genIntBinop subExp subExp]
  where subExp = noLoc <$> (genIntExp' c (n `div` 2))

genBoolExp :: Ctxt -> Gen Exp
genBoolExp c = sized (genBoolExp' c)

genBoolExp' :: Ctxt -> Int -> Gen Exp
genBoolExp' c 0 =
  let arbGen = CBool <$> arbitrary in
  let idGens = return <$> (idsOfType "TBool" c) in
  oneof $ [arbGen] ++ idGens
genBoolExp' c n | n > 0 =
  oneof [genBoolExp' c 0                          ,
         liftM2 Uop genBoolUnop boolExp           ,
         liftM3 Bop genBoolBinop boolExp boolExp  ,
         liftM3 Bop genOrdCompBinop intExp intExp , -- TODO: maybe not just ints
         liftM3 Bop genEqCompBinop intExp intExp  ,
         liftM3 Bop genEqCompBinop boolExp boolExp] -- TODO: add other comps
  where boolExp = noLoc <$> (genBoolExp' c n')
        intExp  = noLoc <$> (genIntExp'  c n')
        n'      = n `div` 2


genIdExpOpt :: Ctxt -> Maybe (Gen Exp)
genIdExpOpt c =
  let ids = concatMap (\k -> vars c ! k) (keys (vars c)) in
  case ids of
    []    -> Nothing
    x:xs  -> Just $ Ast.Id <$> (elements ids)

genExpOfType :: Ctxt -> Ty -> Gen Exp
genExpOfType c ty =
  case ty of
    TInt     -> genIntExp c
    TBool    -> genBoolExp c
    TRef rty -> error "not generating expressions for function types"

-- WIP: getting the call expressions in there
idsForTys :: Ctxt -> [Ty] -> [Maybe (Gen Exp)]
idsForTys c tys =
  do ty <- tys
     case Map.lookup (show ty) (vars c) of
       Nothing  -> return Nothing
       Just ids -> return $ Just $ elements $ Ast.Id <$> ids


--------------------------------------
-- TYPE GENERATORS -------------------
--------------------------------------

genTy :: Gen Ty
genTy = sized genTy'

genTy' :: Int -> Gen Ty
genTy' 0 = oneof $ return <$> [TBool, TInt]
genTy' n | n > 0 =
  oneof [return TBool                          ,
         return TInt                           ,
         TRef <$> (genRty' (n `div` 2 `mod` 3))]

genRty :: Gen Rty
genRty = sized genRty'

genRty' :: Int -> Gen Rty
genRty' n = liftM2 RFun (listOf $ genTy' n) genRetty

genRetty :: Gen Retty
genRetty =
  oneof $ return <$> [RetVoid     ,
                      RetVal TInt ,
                      RetVal TBool]

-- | old version could return function types, but didn't make sense
-- genRetty' :: Int -> Gen Retty
-- genRetty' n =
--  frequency [(4, RetVal <$> (genTy' n)),
--             (1, return RetVoid       )]


--------------------------------------
-- STATEMENT GENERATORS  -------------
--------------------------------------

genMaybeNodeStmt :: Ctxt -> Gen (Maybe (Node Stmt))
genMaybeNodeStmt c =
  oneof [Just <$> genNodeStmt c,
         return Nothing        ]

genNodeStmt :: Ctxt -> Gen (Node Stmt)
genNodeStmt c = sized (genNodeStmt' c)

genNodeStmt' :: Ctxt -> Int -> Gen (Node Stmt)
genNodeStmt' c n = noLoc <$> (genStmt' c n)

genStmt :: Ctxt -> Gen Stmt
genStmt c = sized (genStmt' c)

genStmt':: Ctxt -> Int -> Gen Stmt
genStmt' c 0 =
  let vdecl = [Decl <$> (genVdecl c)] in
  let assn = case genIdExpOpt c of
               Nothing    -> []
               Just idGen -> [liftM2 Assn (nol idGen) (genNodeExp c)] in
  let ret = case currentRetty c of
              RetVoid   -> []
              RetVal ty -> [fmap (Ret . Just) (nol $ genExpOfType c ty)] in
  oneof $ vdecl ++ assn ++ ret

{--
 - So, a few things:
 - the left exp on Assn needs to update the context
 - genVdecl needs to create a new valid id and update the context
 - Ret needs to know the return type and create the proper expression
--}

genVdecl :: Ctxt -> Gen Vdecl
genVdecl c = liftM2 Vdecl arbitrary (genNodeExp c)

{--
genBlock :: Gen Block
genBlock = sized genBlock'

genBlock' :: Int -> Gen Block
genBlock' 0 =  return []
genBlock' n | n > 0 =
  vectorOf n' (genNodeStmt' n')
  where n' = n `div` 2

genStmt' :: Vars -> Int -> Gen Stmt
genStmt' vars 0 =
  oneof [liftM2 Assn genNodeExp genNodeExp            ,
         fmap   Decl genVdecl                         ,
         fmap   Ret  genNodeExp                       ,
         liftM3 If   genNodeEx genBlock genBlock      ,
         liftM4 For  (listOf genVdecl) genMaybeNodeExp
                     genMaybeNodeStmt  genBlock       ]

genArgs :: Gen [(Ty, Id)]
genArgs = listOf genArg

genArg :: Gen (Ty, Id)
genArg = liftM2 (,) genTy arbitrary

genFdecl :: Gen Fdecl
genFdecl = liftM4 Fdecl genRetty arbitrary genArgs genBlock

genDecl :: Gen Decl
genDecl = Gfdecl . noLoc <$> genFdecl

genProg :: Gen Prog
genProg = listOf genDecl

--}
