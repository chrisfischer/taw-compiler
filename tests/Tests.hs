{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Foreign.Ptr
import Data.IORef
import Data.Word
import Data.Int
import qualified Data.ByteString.Short as BS
import qualified Data.ByteString.Char8 as BS8

import LLVM.AST.Global
import LLVM.AST.Constant
import qualified LLVM.Module as M
import qualified LLVM.Context as C
import qualified LLVM.AST as AST
import qualified LLVM.ExecutionEngine as EE

-- import Ast
import JIT

int :: AST.Type
int = AST.IntegerType 64

defAdd :: AST.Definition
defAdd = AST.GlobalDefinition AST.functionDefaults
  { name = AST.Name "main"
  , parameters = ([] , False)
  , returnType = int
  , basicBlocks = [body]
  }
  where
    body = BasicBlock
        (AST.Name "entry")
        []
        (AST.Do $ AST.Ret (Just (AST.ConstantOperand (Int 64 42))) [])


module_ :: AST.Module
module_ = AST.defaultModule
  { AST.moduleName = "basic"
  , AST.moduleDefinitions = [defAdd]
  }


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


main :: IO ()
main = do
  res <- runJIT module_ "main" False
  putStrLn "Eager JIT Result:"
  print res