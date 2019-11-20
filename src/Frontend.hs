module Frontend where

import LLVM.Module
import LLVM.Context

import qualified LLVM.AST as AST
-- import qualified LLVM.AST.Constant as C
-- import qualified LLVM.AST.Float as F
import qualified LLVM.AST.IntegerPredicate as IP

import qualified LLVMGen as L
import qualified Ast as T

-- | Compile a binary operator in a function that takes in LLVM operands
cmpBinop :: T.Binop ->
            (AST.Operand -> AST.Operand -> L.FunctionGen AST.Operand)
cmpBinop T.Add  = L.iadd
cmpBinop T.Sub  = L.isub
cmpBinop T.Mul  = L.imul
cmpBinop T.Div  = L.idiv
cmpBinop T.Eq   = L.icmp IP.EQ
cmpBinop T.Neq  = L.icmp IP.NE
cmpBinop T.Lt   = L.icmp IP.SLT
cmpBinop T.Lte  = L.icmp IP.SLE
cmpBinop T.Gt   = L.icmp IP.SGT
cmpBinop T.Gte  = L.icmp IP.SGE
cmpBinop T.And  = L.band
cmpBinop T.Or   = L.bor
cmpBinop T.IAnd = L.iand
cmpBinop T.IOr  = L.ior
cmpBinop T.Shl  = L.ilshift
cmpBinop T.Shr  = L.irshift
cmpBinop T.Mod  = L.imod

-- | Compile a unary operator in a function that takes in an LLVM operand
cmpUnop :: T.Unop -> (AST.Operand -> L.FunctionGen AST.Operand)
cmpUnop T.Neg    = L.ineg
cmpUnop T.Lognot = L.bnot

-- | Compile a T expression
cmpExpr :: T.Exp -> L.FunctionGen AST.Operand
cmpExpr (T.CBool b) = undefined
cmpExpr (T.CInt i) = undefined
cmpExpr (T.Id id) = undefined
cmpExpr (T.Call (T.Node f _) args) = undefined
cmpExpr (T.Bop b (T.Node e1 _) (T.Node e2 _)) = do
  e1' <- cmpExpr e1
  e2' <- cmpExpr e2
  cmpBinop b e1' e2'
cmpExpr (T.Uop u (T.Node e _)) = do
  e' <- cmpExpr e
  cmpUnop u e'

-- | Compile a T statement
cmpStmt :: T.Stmt -> L.FunctionGen ()
cmpStmt (T.Assn (T.Node e1 _) (T.Node e2 _)) = undefined
cmpStmt (T.Decl (T.Vdecl id (T.Node e _))) = undefined
cmpStmt (T.Ret (T.Node e _)) = undefined
cmpStmt (T.If (T.Node e _) b1 b2) = undefined
cmpStmt (T.For vs cond iter b) = undefined
cmpStmt (T.While (T.Node e _) b) = undefined
cmpStmt T.Nop = return ()

-- | Compile a T block
cmpBlock :: T.Block -> L.FunctionGen ()
cmpBlock = mapM_ (\(T.Node s _) -> cmpStmt s)

-- | Compile a function declaration
cmpFdecl :: T.Fdecl -> L.LLVM()
cmpFdecl fdecl = undefined

-- | Compile an external function reference
cmpFext :: T.Fext -> L.LLVM()
cmpFext fext = undefined

-- | Compile a T program
cmpProg :: T.Prog -> L.LLVM()
cmpProg = undefined
