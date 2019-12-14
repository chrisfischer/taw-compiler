module Main where

import System.FilePath
import System.Environment

import Ast (entryFunctionName)
import Interpreter (run)
import Parser (parseFile)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "error: no input files"
    (fileName : _) -> do
      p <- parseFile fileName
      run entryFunctionName p


main2 fileName = do
  p <- parseFile fileName
  run entryFunctionName p
