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
data CallTy = CallTy Id [Ty] Ty deriving Show

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
  , funs :: Map.Map Ty [FunTy]
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


-- newtype AstGenerator a =
--  AstGenerator { runAstGenerator :: QCT.GenT (State GlobalContext) a }
--  deriving (Functor, Applicative, Monad, QCT.MonadGen, MonadState GlobalContext)

{--
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
--}

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
  in GlobalContext fn ctxts fds

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

-- TODO: finish this associated with genAssn
-- | Finds the first function text where the argued Id was declared
-- or Nothing if no such context can be found
--findSubContextWithId :: (QCT.MonadGen m, MonadState GlobalContext m) =>
--                        Id -> m  

-- | Updates the binding for the argued Id in the first context where it appears
--assnVar :: (QCT.MonadGen m, MonadState GlobalContext m) =>
--           Id -> Ty -> m ()
--assnVar id ty = do
--  curr <- currentFunctionContext
--  let currSub = currSubCtxt curr
--      subWithId = findSubcontextWithId id
--      oldTy = (vars subWithId) ! id
--      vars' = Map.adjust (\_ -> ty) id (vars subWithId)
--      tysToVars' = Map.adjust (ids -> Just $ List.delete id ids) oldTy (tysToVars subWithId)
--      tysToVars'' = Map.adjust (ids -> Just $ id:ids) ty tysToVars'
      

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


--------------------------------------
-- GENERATORS ------------------------
--------------------------------------

-- Operator Generators

genBoolUnop :: (QCT.MonadGen m, MonadState GlobalContext m) => m Unop
genBoolUnop = QCT.liftGen $ return Ast.Neg

genBoolBinop :: (QCT.MonadGen m, MonadState GlobalContext m) => m Binop
genBoolBinop = QCT.liftGen $ elements [Ast.And, Ast.Or]

genIntBinop :: (QCT.MonadGen m, MonadState GlobalContext m) => m Binop
genIntBinop = QCT.liftGen $ elements [Ast.Add, Ast.Sub, Ast.Mul, Ast.Div, Ast.Mod]
-- ^ TODO: add logical operators here

genOrdCompBinop :: (QCT.MonadGen m, MonadState GlobalContext m) => m Binop
genOrdCompBinop = QCT.liftGen $ elements [Ast.Lt, Ast.Lte, Ast.Gt, Ast.Gte]

genEqCompBinop :: (QCT.MonadGen m, MonadState GlobalContext m) => m Binop
genEqCompBinop = QCT.liftGen $ elements [Ast.Eq, Ast.Neq]

-- Expression Generators

-- | Generates an expresssion of an argued type
genExpOfTy :: (QCT.MonadGen m, MonadState GlobalContext m) => Ty -> m Exp
genExpOfTy ty = case ty of
                  TBool -> genBoolExp
                  TInt  -> genIntExp
                  _     -> error "Invalid type argued" 

-- | generate a correct expression of type TBool
genBoolExp :: (QCT.MonadGen m, MonadState GlobalContext m) => m Exp
genBoolExp = QCT.sized genBoolExp'

genBoolExp' :: (QCT.MonadGen m, MonadState GlobalContext m) => Int -> m Exp
genBoolExp' 0 = do
  ids   <- inScopeVarsWithType TBool
  QCT.liftGen $ oneof [CBool <$> arbitrary  , 
                       elements $ Id <$> ids]
genBoolExp' n | n > 0 =
  QCT.oneof [genBoolExp' 0                            ,
             liftM2 Uop genBoolUnop boolExp           ,
             liftM3 Bop genBoolBinop boolExp boolExp  ,
             liftM3 Bop genOrdCompBinop intExp intExp ,
             liftM3 Bop genEqCompBinop intExp intExp  ,
             liftM3 Bop genEqCompBinop boolExp boolExp,
             genCallExpWithType TBool                 ]
  where boolExp = noLoc <$> (genBoolExp' n')
        intExp  = noLoc <$> (genIntExp'  n')
        n'      = n `div` 2

-- | generate a correct expression of type TInt
genIntExp :: (QCT.MonadGen m, MonadState GlobalContext m) => m Exp
genIntExp = QCT.sized genIntExp'

genIntExp' :: (QCT.MonadGen m, MonadState GlobalContext m) => Int -> m Exp
genIntExp' 0 = do
  ids <- inScopeVarsWithType TInt
  QCT.liftGen $ oneof [CInt <$> arbitrary, elements $ Id <$> ids]
genIntExp' n =
  QCT.oneof [genIntExp' 0,
             liftM3 Bop genIntBinop subExp subExp,
             genCallExpWithType TInt             ]
  where subExp = noLoc <$> (genIntExp' (n `div` 2))

-- | Generates a correct Call expression for a function that returns the argued type
genCallExpWithType :: (QCT.MonadGen m, MonadState GlobalContext m) => Ty -> m Exp
genCallExpWithType ty = do
  callTys <- inScopeCallTysWithRetType ty
  index   <- QCT.liftGen $ choose (0, length callTys)
  let (CallTy id tys rty) = callTys !! index
  bools   <- QCT.vectorOf (length tys) genBoolExp
  ints    <- QCT.vectorOf (length tys) genIntExp
  let args = map (getVal bools ints) (zip tys [0..(length tys)])
  return $ Call (noLoc $ Id id) (noLoc <$> args)
  where
    getVal :: [Exp] -> [Exp] -> (Ty, Int) -> Exp
    getVal bools ints (ty, i) = case ty of
                                  TBool -> bools !! i
                                  TInt  -> ints !! i
                                  _     -> error "whoops, can't handle function types yet"

-- Type Generators

-- | Generate an arbitrary primitive Ty
genPrimitiveTy :: (QCT.MonadGen m, MonadState GlobalContext m) => m Ty
genPrimitiveTy = QCT.elements [TBool, TInt]

-- | Generate an arbitrary Ty
genTy :: (QCT.MonadGen m, MonadState GlobalContext m) => m Ty
genTy = QCT.sized genTy'

genTy' :: (QCT.MonadGen m, MonadState GlobalContext m) => Int -> m Ty
genTy' 0 = genPrimitiveTy
genTy' n | n > 0 = 
  QCT.oneof [genTy' 0                              ,
             TRef <$> (genRty' (n `div` 2 `mod` 3))] -- TODO: improve this limit strategy

-- | Generate an arbitrary Rty (function type)
genRty :: (QCT.MonadGen m, MonadState GlobalContext m) => m Rty
genRty = QCT.sized genRty'

genRty' :: (QCT.MonadGen m, MonadState GlobalContext m) => Int -> m Rty
genRty' n = liftM2 RFun (QCT.listOf $ genTy' n) genRetty

-- | Generate an arbitrary return type
genRetty :: (QCT.MonadGen m, MonadState GlobalContext m) => m Retty
genRetty = RetVal <$> genPrimitiveTy

-- Statement Generators

genStmt :: (QCT.MonadGen m, MonadState GlobalContext m) => m Stmt
genStmt = undefined

genAssnStmt :: (QCT.MonadGen m, MonadState GlobalContext m) => m Stmt
genAssnStmt = do
  varsToTys <- inScopeVars 
  index     <- QCT.liftGen $ choose (0, (Map.size varsToTys))
  (id, ty)  <- (Map.elems varsToTys) !! index
  ty'       <- genPrimitiveTy
  exp       <- genExpOfTy ty'
  if ty == ty' 
  then return $ Assn (noLoc $ id) (noLoc exp)
  else error "need to implement the context updating" -- TODO

genVdeclWithType :: (QCT.MonadGen m, MonadState GlobalContext m) => m (Ty, Vdecl)
genVdeclWithType = do
  id  <- genId
  ty  <- genPrimitiveTy
  exp <- genExpOfTy ty
  return $ (ty, Vdecl id (noLoc exp))

-- Helpers

-- | Generate an arbitrary variable name TODO: readd that library I was using for this
genId :: (QCT.MonadGen m, MonadState GlobalContext m) => m Id
genId = QCT.arbitrary'

{--
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


genVdecl :: Ctxt -> Gen Vdecl
genVdecl c = liftM2 Vdecl arbitrary (genNodeExp c)
--}
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
