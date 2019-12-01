module AstGen where

import Ast
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Gen

--------------------------------------
-- CONTEXTS --------------------------
--------------------------------------
-- TODO

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
genExp :: Gen Exp
genExp = sized genExp'

genExp' :: Int -> Gen Exp
genExp' n =
  oneof [genIntExp'  n,
         genBoolExp' n]
--         genCallExp   ,
--         genIdExp     ]

genIntExp :: Gen Exp
genIntExp = sized genIntExp'

genIntExp' :: Int -> Gen Exp
genIntExp' 0 = CInt <$> arbitrary
genIntExp' n | n > 0 =
  oneof [genIntExp' 0,
         liftM3 Bop genIntBinop subExp subExp]
  where subExp = noLoc <$> (genIntExp' (n `div` 2))

genBoolExp :: Gen Exp
genBoolExp = sized genBoolExp'

genBoolExp' :: Int -> Gen Exp
genBoolExp' 0 = CBool <$> arbitrary
genBoolExp' n | n > 0 =
  oneof [genBoolExp' 0                            ,
         liftM2 Uop genBoolUnop boolExp           ,
         liftM3 Bop genBoolBinop boolExp boolExp  ,
         liftM3 Bop genOrdCompBinop intExp intExp , -- TODO: maybe not just ints
         liftM3 Bop genEqCompBinop intExp intExp  ,
         liftM3 Bop genEqCompBinop boolExp boolExp] -- TODO: add other comps
  where boolExp = noLoc <$> (genBoolExp' n')
        intExp  = noLoc <$> (genIntExp'  n')
        n'      = n `div` 2

--------------------------------------
-- TYPE GENERATORS -------------------
--------------------------------------


-------------------------------------
-- OLD ------------------------------
-------------------------------------
genFromEnum :: Enum a => a -> Gen a
genFromEnum e = elements $ enumFrom e

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
genRty' n = liftM2 RFun (listOf $ genTy' n) (genRetty' n)

genRetty :: Gen Retty
genRetty = sized genRetty'

genRetty' :: Int -> Gen Retty
genRetty' n = 
  frequency [(4, RetVal <$> (genTy' n)), 
             (1, return RetVoid       )] 

-- genValueTy :: Gen ValueTy
-- genValueTy =
--  oneof [VBool <$> arbitrary, 
--         VInt  <$> arbitrary,
--         VFun  <$> arbitrary]

genMaybeNodeExp :: Gen (Maybe (Node Exp))
genMaybeNodeExp =
  oneof [Just <$> genNodeExp,
         return Nothing     ]

genNodeExp :: Gen (Node Exp)
genNodeExp = noLoc <$> genExp

{-
genExp :: Gen Exp
genExp = sized genExp'

genExp' :: Int -> Gen Exp
genExp' 0 = 
  oneof [CBool <$> arbitrary,
         CInt  <$> arbitrary,
         Id    <$> arbitrary]
genExp' n | n > 0 =
  oneof [genBoolExp' n                        ,
         fmap   CInt  arbitrary               ,
         fmap   Id    arbitrary               ,
         liftM3 Bop   genBinop subExp subExp  ,
         liftM2 Uop   genUnop  subExp         ,
         liftM2 Call  subExp   (listOf subExp)]
    where subExp = noLoc <$> genExp' (n `div` 2)

-}
genVdecl :: Gen Vdecl
genVdecl = liftM2 Vdecl arbitrary genNodeExp

genBlock :: Gen Block
genBlock = sized genBlock'

genBlock' :: Int -> Gen Block
genBlock' 0 =  return []
genBlock' n | n > 0 =
  vectorOf n' (genNodeStmt' n')
  where n' = n `div` 2

genMaybeNodeStmt :: Gen (Maybe (Node Stmt))
genMaybeNodeStmt =
  oneof [Just <$> genNodeStmt,
         return Nothing      ]

genNodeStmt :: Gen (Node Stmt)
genNodeStmt = sized genNodeStmt'

genNodeStmt' :: Int -> Gen (Node Stmt)
genNodeStmt' n = noLoc <$> (genStmt' n)

genStmt :: Gen Stmt
genStmt = sized genStmt'

genStmt' :: Int -> Gen Stmt
genStmt' 0 = 
  oneof [liftM2 Assn genNodeExp genNodeExp            ,
         fmap   Decl genVdecl                         ,
         fmap   Ret  genNodeExp                       , 
         liftM3 If   genNodeExp genBlock genBlock     ,
         liftM4 For  (listOf genVdecl) genMaybeNodeExp
                     genMaybeNodeStmt  genBlock       ]

genArgs :: Gen [(Ty, Id)]
genArgs = listOf genArg

genArg :: Gen (Ty, Id)
genArg = liftM2 (,) genTy arbitrary

genFdecl :: Gen Fdecl
genFdecl = liftM4 Fdecl genRetty arbitrary genArgs genBlock

genFext :: Gen Fext
genFext = liftM3 Fext genRetty arbitrary genArgs

genDecl :: Gen Decl
genDecl = Gfdecl . noLoc <$> genFdecl
--  oneof [Gfdecl . noLoc <$> genFdecl,
--         Gfext  . noLoc <$> genFext ]

genProg :: Gen Prog
genProg = listOf genDecl

