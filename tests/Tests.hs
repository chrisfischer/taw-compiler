{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Data.Int

import Control.Monad.IO.Class

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Ast
import JIT (runJIT)
import Frontend (cmpProg, idToShortBS)
import Interpreter (executeProg, ValueTy(VInt))

-- int :: AST.Type
-- int = AST.IntegerType 64

-- defAdd :: AST.Definition
-- defAdd = AST.GlobalDefinition AST.functionDefaults
--   { name = AST.Name "main"
--   , parameters = ([] , False)
--   , returnType = int
--   , basicBlocks = [body]
--   }
--   where
--     body = BasicBlock
--         (AST.Name "entry")
--         []
--         (AST.Do $ AST.Ret (Just (AST.ConstantOperand (Int 64 42))) [])


-- module_ :: AST.Module
-- module_ = AST.defaultModule
--   { AST.moduleName = "basic"
--   , AST.moduleDefinitions = [defAdd]
--   }


-- idd x = noLoc $ Id x

-- testFDecl2 = Fdecl (RetVal TInt) "add" [(TInt, "x"), (TInt, "y")]
--   [ noLoc $ Decl $ Vdecl "z" $ noLoc $ Bop Add (idd "x") (idd "y")
--   , noLoc $ Ret $ noLoc (Id "z") ]

-- testFDecl3 = Fdecl (RetVal TInt) "do" [(TRef (RFun [TInt, TInt] (RetVal TInt)), "f"), (TInt, "x"), (TInt, "y")]
--   [ noLoc $ Decl $ Vdecl "z" $ noLoc $ Call (idd "f") [(idd "x"), (idd "y")]
--   , noLoc $ Ret $ noLoc (Id "z") ]

-- testFDecl = Fdecl (RetVal TInt) "main" []
--   [ noLoc $ Decl $ Vdecl "x" $ noLoc $ CInt 42
--   , noLoc $ Decl $ Vdecl "f" $ noLoc $ Id "add"
--   , noLoc $ Decl $ Vdecl "y" $ noLoc $ Call (idd "do") [(idd "f"), (idd "x"), (idd "x")]
--   , noLoc $ Decl $ Vdecl "z" $ noLoc $ Bop Add (idd "x") (idd "y")
--   , noLoc $ Ret $ idd "z" ]

-- testProg = [
--     Gfdecl $ noLoc testFDecl2
--   , Gfdecl $ noLoc testFDecl3
--   , Gfdecl $ noLoc testFDecl ]

prop_interpCmp :: Prog -> Property
prop_interpCmp p = monadicIO $ do
  case cmpProg "module" p of
    Left err -> error err
    Right ll -> do
      let resInterp = executeProg p entryFunctionName
      resCmp <- liftIO $ runJIT ll (idToShortBS entryFunctionName) False
      case (resInterp, resCmp) of
        (Right (VInt v1), Right v2) -> assert $ v1 == fromIntegral v2
        _ -> error "Bad"


main :: IO ()
main = return () -- quickCheckWith stdArgs { maxSuccess = 1 } prop_interpCmp
