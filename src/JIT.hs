{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module JIT (runJIT) where

import Control.Monad
import Control.Exception

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
  mkMain :: FunPtr (IO Int64) -> IO Int64

run :: FunPtr a -> IO Int64
run fn = mkMain (castFunPtr fn :: FunPtr (IO Int64))

defaultJit :: C.Context -> (EE.MCJIT -> IO a) -> IO a
defaultJit c = EE.withMCJIT c (Just 0) Nothing Nothing Nothing

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

-- | Executes the given LLVM module, starting at the given function name
runJIT :: AST.Module -> BS.ShortByteString -> Bool -> IO (Either String Int64)
runJIT mod fentry verbose =
  C.withContext $ \context ->
    defaultJit context $ \executionEngine ->
      M.withModuleFromAST context mod $ \m ->
        EE.withModuleInEngine executionEngine m $ \ee -> do
          mainfn <- EE.getFunction ee $ AST.Name fentry
          when verbose $ do
            s <- M.moduleLLVMAssembly m
            BS8.putStrLn s
          case mainfn of
            Just fn ->
              fmap Right (run fn) `catchAny` \e -> return $ Left $ show e
            Nothing ->
              return $ Left $ "entry function " ++ show fentry ++ " not found"
