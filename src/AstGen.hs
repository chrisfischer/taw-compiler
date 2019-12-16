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

nol x = noLoc <$> x

emptyFdecl r fn as = Fdecl r fn as []

-- | The type of the main function in every possible program
mainTy :: FunTy
mainTy = FunTy "main" [] TInt

-- variables for testing
funTys :: [FunTy]
funTys = [ --FunTy "f1" [(TInt, "a1"), (TInt, "a2")] TInt
         --  FunTy "f2" [(TBool, "b1")] TBool
         --, FunTy "f3" [(TBool, "c1"), (TBool, "c2")] TBool
         --, FunTy "f4" [(TBool, "d1"), (TBool, "d2")] TBool
         -- FunTy "f5" [(TRef $ RFun [] (RetVal TBool), "e1_f")] TBool
          FunTy "f6" [(TRef $ RFun [TBool] (RetVal TBool), "f1_f")] TBool
         ]

run = sample $ execAstGenerator mainTy funTys
run' = generate $ progGen -- execAstGenerator mainTy funTys

-- Name, arg types and names, return type (must not be void)
data FunTy = FunTy Id [(Ty, Id)] Ty deriving Show
data CallTy = CallTy Id [Ty] Ty deriving Show

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

-- Entry Points

instance Arbitrary Prog where
  arbitrary = progGen -- execAstGenerator mainTy funtys


-- | Gen Prog
progGen :: Gen Prog
progGen = do
  numFuns <- choose (0, 2) -- maximum 2 functions
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
genIntBinop = QCT.liftGen $ elements [Ast.Add, Ast.Sub, Ast.Mul]
<<<<<<< HEAD
-- ^ TODO: add logical operators here
=======
-- ^ TODO: add logical operators here, readd Div and Mod , Ast.Div, Ast.Mod
>>>>>>> WIP

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
               liftM3 Bop genEqCompBinop boolExp boolExp]
              ++ if canCall then [genCallExpWithType TBool] else []
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
    bools   <- QCT.vectorOf (length tys) genBoolExp
    ints    <- QCT.vectorOf (length tys) genIntExp
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


-- Type Generators

-- | Generate an arbitrary primitive Ty
genPrimitiveTy :: (QCT.MonadGen m, MonadState GlobalContext m) => m Ty
genPrimitiveTy = QCT.elements [TBool, TInt]

-- | Generate an arbitrary Ty
genTy :: Gen Ty
genTy = QCT.sized genTy'

genTy' :: Int -> Gen Ty
genTy' 0 = genPrimTy
genTy' n | n > 0 =
  QCT.oneof [genTy' 0                              ,
             TRef <$> (genRty' (n `div` 2 `mod` 3))] -- TODO: improve this limit strategy

-- | Generate an arbitrary Rty (function type)
genRty :: Gen Rty
genRty = QCT.sized genRty'

genRty' :: Int -> Gen Rty
genRty' n = liftM2 RFun (QCT.listOf $ genTy' n) genRetty

-- | Generate an arbitrary return type
genRetty :: Gen Retty
genRetty = RetVal <$> genPrimTy

-- | Generate FunTy that is a good candidate for our context
-- in that any arguments that are functions will take only
-- primitives for their arguments
genFunTy :: Gen FunTy
genFunTy = do
  funId   <- genFreshId'
  numArgs <- choose (0, 10) -- limit to 10 args
  args    <- vectorOf numArgs genArg
  rty     <- genPrimTy
  return $ FunTy funId args rty

-- | Generate an arg that's either a primitive type or
-- a function whose arguments are all primitives
genArg :: Gen (Ty, Id)
genArg = do
  ty <- frequency [(1, genSimpleTRef), (4, genPrimTy)]
  id <- genFreshId'
  return $ (ty, id)

-- | Generate a function type Rty whose arguments are all primitives
genSimpleTRef :: Gen Ty
genSimpleTRef = do
  numArgs <- choose (0, 10)
  args    <- vectorOf numArgs genPrimTy
  rty     <- genPrimTy
  return $ TRef $ RFun args (RetVal rty)

-- | Generate a FunTy whose arguments are all primitives
genSimpleFunTy :: Gen FunTy
genSimpleFunTy = do
  funId   <- genFreshId'
  numArgs <- choose (0, 10) -- limit to 10 args
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
  QCT.listOf $ QCT.oneof [genSimpleStmt, genIfStmt, genWhileStmt, genForStmt]
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
-- TODO: be able to change var types and update context accordingly
--  varsToTys <- inScopeVars
--  index     <- QCT.liftGen $ choose (0, (Map.size varsToTys))
--  (id, ty)  <- (Map.elems varsToTys) !! index
--  ty'       <- genPrimitiveTy
--  exp       <- genExpOfTy ty'
--  if ty == ty'
--  then return $ Assn (noLoc $ id) (noLoc exp)
--  else error "need to implement the context updating" -- TODO

-- | Generate a declaration statement, update the state to include
-- the new variable, and push the statement to the context
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
  QCT.listOf genSimpleStmt
  ifBlock        <- popSubContext
  elseMaybeBlock <- genMaybeBlock
  pushStmt $ If (noLoc condition) ifBlock elseMaybeBlock

-- | Generate a maybe block - an arbitrary choice between Nothing and
-- Just some arbitrary block
genMaybeBlock :: (QCT.MonadGen m, MonadState GlobalContext m) => m (Maybe Block)
genMaybeBlock = do
  pushSubContext -- TODO: refactor so we only do the work if necessary
  QCT.listOf genSimpleStmt
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
  QCT.listOf genSimpleStmt
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
  QCT.listOf genSimpleStmt
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

-- | Generate a fresh arbitrary variable name TODO: readd that library I was using for this
genFreshId :: (QCT.MonadGen m, MonadState GlobalContext m) => m Id
genFreshId = QCT.liftGen $ unpack <$> (matchRegexp $ pack "[a-z]\\w+") -- QCT.arbitrary'

genFreshId' :: Gen Id
genFreshId' = unpack <$> (matchRegexp $ pack "[a-z]\\w+") -- QCT.arbitrary'


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
  let assn = case genFreshIdExpOpt c of
               Nothing    -> []
               Just idGen -> [liftM2 Assn (nol idGen) (genNodeExp c)] in
  let ret = case currentRetty c of
              RetVoid   -> []
              RetVal ty -> [fmap (Ret . Just) (nol $ genExpWithType c ty)] in
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
