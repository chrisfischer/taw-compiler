{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Tests where

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

foreign import ccall "dynamic"
  mkMain :: FunPtr (IO Int64) -> (IO Int64)

run :: FunPtr a -> IO Int64
run fn = mkMain (castFunPtr fn :: FunPtr (IO Int64))

defaultJit :: C.Context -> (EE.MCJIT -> IO a) -> IO a
defaultJit c = EE.withMCJIT c (Just 0) Nothing Nothing Nothing

-- | Executes the given LLVM module, starting at the given function name
runJIT :: AST.Module -> BS.ShortByteString -> IO (Either String Int64)
runJIT mod fentry = do
  C.withContext $ \context ->
    defaultJit context $ \executionEngine ->
      M.withModuleFromAST context mod $ \m ->
        EE.withModuleInEngine executionEngine m $ \ee -> do
          mainfn <- EE.getFunction ee $ AST.Name fentry
          -- s <- M.moduleLLVMAssembly m
          -- BS8.putStrLn s
          case mainfn of
            Just fn -> do
              res <- run fn
              return $ Right res
            Nothing ->
              return $ Left $ show $ "Entry function not found: " <> fentry


-- instance Arbitrary Syntax.Prog where
--   arbitrary = undefined
--   shrink = undefined


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

main :: IO ()
main = do
  res <- runJIT module_ "main"
  putStrLn "Eager JIT Result:"
  print res