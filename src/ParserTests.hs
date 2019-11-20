module ParserTest where

import Ast
import Parser (node)
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Gen

genFromEnum :: Enum a => a -> Gen a
genFromEnum e = elements $ enumFrom e

genBinop :: Gen Binop
genBinop = genFromEnum Add

genUnop :: Gen Unop
genUnop = genFromEnum Neg

genTy :: Gen Ty
genTy = genFromEnum TBool

genRetty :: Gen Retty
genRetty = 
  oneof [RetVal <$> genTy  , 
         elements [RetVoid]] -- TODO: what's the better way?

genValueTy :: Gen ValueTy
genValueTy =
  oneof [VBool <$> arbitrary, 
         VInt  <$> arbitrary,
         VFun  <$> arbitrary]

genMaybeNodeExp :: Gen (Maybe (Node Exp))
genMaybeNodeExp =
  oneof [Just <$> genNodeExp,
         elements [Nothing] ] -- TODO: here, too

genNodeExp :: Gen (Node Exp)
genNodeExp = node <$> genExp

genExp :: Gen Exp
genExp = sized genExp'

genExp' :: Int -> Gen Exp
genExp' 0 = 
  oneof [CBool <$> arbitrary,
         CInt  <$> arbitrary,
         Id    <$> arbitrary]
genExp' n | n > 0 =
  oneof [fmap   CBool arbitrary               ,
         fmap   CInt  arbitrary               ,
         fmap   Id    arbitrary               ,
         liftM3 Bop   genBinop subExp subExp  ,
         liftM2 Uop   genUnop  subExp         ,
         liftM2 Call  subExp   (listOf subExp)]
    where subExp = node <$> genExp' (n `div` 2)

genVdecl :: Gen Vdecl
genVdecl = liftM2 Vdecl arbitrary genNodeExp

genBlock :: Gen Block
genBlock = listOf genNodeStmt

genMaybeNodeStmt :: Gen (Maybe (Node Stmt))
genMaybeNodeStmt =
  oneof [Just <$> genNodeStmt,
         elements [Nothing]  ] -- TODO: here, too

genNodeStmt :: Gen (Node Stmt)
genNodeStmt = node <$> genStmt

genStmt :: Gen Stmt
genStmt =
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
genDecl =
  oneof [Gfdecl . node <$> genFdecl,
         Gfext  . node <$> genFext ]

genProg :: Gen Prog
genProg = listOf genDecl

