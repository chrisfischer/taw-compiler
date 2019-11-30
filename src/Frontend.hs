{-# LANGUAGE OverloadedStrings #-}

module Frontend where

import Control.Applicative
import Control.Monad
import Control.Monad.State

import qualified LLVM.AST as AST
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.Module as M
import qualified LLVM.Context as C

import qualified Data.ByteString.Short as BS
import qualified Data.ByteString.Char8 as BS8

import qualified LLVMGen as L
import qualified Ast as T

import Tests (runJIT)
import Ast
import Debug.Trace

-- TODO remove
idToShortBS = BS.toShort . BS8.pack

-- | Compile a binary operator into a function that takes in LLVM operands
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

-- TODO match types of binops?

-- | Compile a unary operator in a function that takes in an LLVM operand
cmpUnop :: T.Unop -> (AST.Operand -> L.FunctionGen AST.Operand)
cmpUnop T.Neg    = L.ineg
cmpUnop T.Lognot = L.bnot

-- | Compile a T expression
cmpExpr :: (T.Node T.Exp) -> L.FunctionGen AST.Operand
cmpExpr (T.Node (T.CBool b) _) = return $ L.booleanConst b
cmpExpr (T.Node (T.CInt i) _) = return $ L.integerConst $ toInteger i
-- TODO lookup function
cmpExpr (T.Node (T.Id id) _) = L.getVar (idToShortBS id) >>= L.load
-- TODO expand to function pointers
cmpExpr (T.Node (T.Call (T.Node (T.Id id) _) args) _) = do
  -- f' <- cmpExpr f
  args' <- mapM cmpExpr args
  -- TODO function lookup for types?
  -- TODO change this hard coding
  fref <- L.globalf (idToShortBS id)
  L.call fref args' L.integer

cmpExpr (T.Node (T.Bop b e1 e2) _) = do
  e1' <- cmpExpr e1
  e2' <- cmpExpr e2
  cmpBinop b e1' e2'
cmpExpr (T.Node (T.Uop u e) _) = do
  e' <- cmpExpr e
  cmpUnop u e'

-- | Compile a T statement
cmpStmt :: (T.Node T.Stmt) -> L.FunctionGen ()
cmpStmt (T.Node (T.Assn (T.Node (T.Id id) _) e2) _) = do
  r <- L.getVar $ idToShortBS id
  newVal <- cmpExpr e2
  L.store r newVal
cmpStmt (T.Node (T.Assn (T.Node (_) _) _) _) = error "Assign called on non id"

cmpStmt (T.Node (T.Decl (T.Vdecl id e)) _) = do
  e' <- cmpExpr e
  let ty = L.typeFromOperand e'
  traceM (show ty)
  r <- L.alloca ty
  traceM (show r)
  L.store e' r
  L.assign (idToShortBS id) r

cmpStmt (T.Node (T.Ret e) _) = do
  e' <- cmpExpr e
  L.ret e'

cmpStmt (T.Node (T.If e b1 b2) _) = do
  thenLbl <- L.addBlock "then"
  elseLbl <- L.addBlock "else"
  exitLbl <- L.addBlock "exit"

  -- %entry
  cond <- cmpExpr e
  test <- L.bneq cond (L.booleanConst False)
  L.cbr test thenLbl elseLbl

  -- then
  L.setCurrentBlock thenLbl
  cmpBlock b1            -- Generate code for the true branch
  L.br exitLbl           -- Branch to the merge block

  -- else
  L.setCurrentBlock elseLbl
  cmpBlock b2            -- Generate code for the false branch
  L.br exitLbl           -- Branch to the merge block

  -- exit
  L.setCurrentBlock exitLbl

cmpStmt (T.Node (T.For vs cond iter b) _) = do
  let cond' = case cond of
                Just e -> e
                Nothing -> T.noLoc $ T.CBool True
  let iter' = case iter of
                Just s -> [s]
                Nothing -> []
  let ds = map (T.noLoc . T.Decl) vs
  cmpBlock (ds ++ [T.noLoc (T.While cond' (b ++ iter'))])

cmpStmt (T.Node (T.While e b) _) = do
  condLbl <- L.addBlock "while"
  loopLbl <- L.addBlock "loop"
  exitLbl <- L.addBlock "exit"

  -- cond
  cond <- cmpExpr e
  test <- L.bneq cond (L.booleanConst False)
  L.cbr test loopLbl exitLbl

  -- loop
  L.setCurrentBlock loopLbl
  cmpBlock b
  L.br condLbl

  -- exit
  L.setCurrentBlock exitLbl

cmpStmt (T.Node T.Nop _) = return ()

-- | Compile a T block
cmpBlock :: T.Block -> L.FunctionGen ()
cmpBlock = mapM_ cmpStmt

cmpTy :: T.Ty -> AST.Type
cmpTy T.TBool = L.boolean
cmpTy T.TInt = L.integer
cmpTy (T.TRef (T.RFun argtys retty)) =
  L.functionPtr (cmpRetty retty) (map cmpTy argtys)

cmpRetty :: T.Retty -> AST.Type
cmpRetty (T.RetVal t) = cmpTy t
cmpRetty T.RetVoid = undefined  -- TODO

-- TODO handle void
-- | Compile a function declaration
cmpDecl :: L.FunctionTypeContext -> T.Decl -> L.LLVM ()
cmpDecl ctxt (T.Gfdecl (T.Node (T.Fdecl (T.RetVal retty) name args body) _)) =
  let args' = map (\(ty, id) -> (cmpTy ty, AST.Name (idToShortBS id))) args
      blocks = L.createBlocks $ L.execFunctionGen $ do
        L.setFtyCtxt ctxt
        entry <- L.addBlock L.entryBlockName
        L.setCurrentBlock entry
        forM_ args' $ \(ty, n@(AST.Name name)) -> do
          v <- L.alloca ty
          L.store v (L.local ty n)
          L.assign name v
        cmpBlock body in
  L.define (cmpTy retty) (idToShortBS name) args' blocks

-- TODO handle void
cmpDecl _ (T.Gfext (T.Node (T.Fext (T.RetVal retty) name args) _)) =
  let args' = map (\(ty, id) -> (cmpTy ty, AST.Name (idToShortBS id))) args in
  L.external (cmpTy retty) (idToShortBS name) args'


-- TYPE PASS

extractDeclTy :: T.Decl -> L.FunctionTypeGen ()
extractDeclTy (T.Gfdecl (T.Node (T.Fdecl retty name args _) _)) = do
  let retty' = cmpRetty retty
      argtys = map (cmpTy . fst) args
      fty = L.functionPtr retty' argtys
  L.setType (idToShortBS name) fty
extractDeclTy (T.Gfext (T.Node (T.Fext retty name args) _)) = do
  let retty' = cmpRetty retty
      argtys = map (cmpTy . fst) args
      fty = L.functionPtr retty' argtys
  L.setType (idToShortBS name) fty

extractTypes :: T.Prog -> L.FunctionTypeContext
extractTypes p = L.execFunctionTypeGen $ mapM_ extractDeclTy p


-- | Compile a Taw program
cmpProg :: T.Prog -> L.LLVM ()
cmpProg p = do
  let ctxt = extractTypes p
  mapM_ (cmpDecl ctxt) p

execCmp :: String -> T.Prog -> AST.Module
execCmp modName p = L.runLLVM (L.emptyModule (idToShortBS modName)) $ cmpProg p

ppModule :: AST.Module -> IO ()
ppModule ast = C.withContext $ \ctx ->
  M.withModuleFromAST ctx ast $ \m -> do
    llstr <- M.moduleLLVMAssembly m
    BS8.putStrLn llstr


idd x = noLoc $ Id x

testFDecl2 = Fdecl (RetVal TInt) "add" []
  [ noLoc $ Decl $ Vdecl "x" $ noLoc $ CInt 42
  , noLoc $ Decl $ Vdecl "y" $ noLoc $ CInt 10
  , noLoc $ Decl $ Vdecl "z" $ noLoc $ Bop Add (idd "x") (idd "y")
  , noLoc $ Ret $ noLoc (Id "z") ]

testFDecl = Fdecl (RetVal TInt) "main" []
  [ noLoc $ Decl $ Vdecl "x" $ noLoc $ CInt 42
  , noLoc $ Decl $ Vdecl "y" $ noLoc $ Call (idd "add") []
  , noLoc $ Decl $ Vdecl "z" $ noLoc $ Bop Add (idd "x") (idd "y")
  , noLoc $ Ret $ idd "z" ]

testProg = [
    Gfdecl $ noLoc testFDecl2
  , Gfdecl $ noLoc testFDecl ]


main :: IO ()
main = do
  let ll = execCmp "test.taw" testProg
  -- putStrLn $ show ll
  ppModule ll
  res <- runJIT ll "main"
  putStrLn "Result: "
  print res
  -- print $ extractTypes testProg