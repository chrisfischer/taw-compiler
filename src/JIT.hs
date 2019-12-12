{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module JIT (runJIT) where

import Control.Monad

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
runJIT :: AST.Module -> BS.ShortByteString -> Bool -> IO (Either String Int64)
runJIT mod fentry verbose = do
  C.withContext $ \context ->
    defaultJit context $ \executionEngine ->
      M.withModuleFromAST context mod $ \m ->
        EE.withModuleInEngine executionEngine m $ \ee -> do
          mainfn <- EE.getFunction ee $ AST.Name fentry
          when verbose $ do
            s <- M.moduleLLVMAssembly m
            BS8.putStrLn s
          case mainfn of
            Just fn -> do
              res <- run fn
              return $ Right res
            Nothing ->
              return $ Left $ show $ "Entry function not found: " <> fentry
