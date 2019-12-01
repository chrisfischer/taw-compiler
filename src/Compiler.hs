module Main where

import System.FilePath
import System.Environment

import qualified LLVM.AST as AST
import qualified LLVM.Module as M
import qualified LLVM.Context as C

import qualified Data.ByteString.Char8 as BS8

import Frontend (execCmp)
import Parser (parseFile)

writeLLVM :: String -> AST.Module -> IO ()
writeLLVM fileName ast = C.withContext $ \ctx ->
  M.withModuleFromAST ctx ast $ \m -> do
    llstr <- M.moduleLLVMAssembly m
    BS8.writeFile fileName llstr

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  p <- parseFile fileName
  let ll = execCmp fileName p
  let newFileName = (fst $ splitExtension fileName) ++ ".ll"
  writeLLVM newFileName ll
