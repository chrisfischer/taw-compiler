module Main where

import System.FilePath
import System.Environment

import qualified LLVM.AST as AST
import qualified LLVM.Module as M
import qualified LLVM.Context as C

import qualified Data.ByteString.Char8 as BS8

import Frontend (cmpProg)
import Parser (parseFileM)

writeLLVM :: String -> AST.Module -> IO ()
writeLLVM fileName ast = C.withContext $ \ctx ->
  M.withModuleFromAST ctx ast $ \m -> do
    llstr <- M.moduleLLVMAssembly m
    BS8.writeFile fileName llstr

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "error: no input files"
    (fileName : _) -> parseFileM fileName $ \p ->
      case cmpProg fileName p of
        Left err -> putStrLn $ "error: " ++ err
        Right ll -> do
          let newFileName = fst (splitExtension fileName) ++ ".ll"
          writeLLVM newFileName ll
