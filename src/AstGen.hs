module AstGen where

import Ast
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Gen
import Data.Map as Map

--------------------------------------
-- UTILS - ---------------------------
--------------------------------------

nol x = noLoc <$> x


--------------------------------------
-- CONTEXT ---------------------------
--------------------------------------

data Ctxt = Ctxt {vars :: Map String [String], -- lookup vars by ty string
                  funs :: Map String [String], -- lookup functions by retty string
                  funTypes :: Map String Rty , -- lookup functions Rtys by id
                  currentFun :: String       }

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
currentRetty c = let RFun _ retty = funTypes c ! (currentFun c) in retty

-- test contexts
myC :: Ctxt
myC = Ctxt (insert "TBool" ["bool1", "bool2"] 
             (insert "TInt" ["int1", "int2"] empty))
           (insert "TBool" ["fBool"] empty)
           (insert "fBool" (RFun [TBool, TBool] (RetVal TBool)) empty)
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
              RetVal ty -> [fmap Ret (nol $ genExpOfType c ty)] in
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
