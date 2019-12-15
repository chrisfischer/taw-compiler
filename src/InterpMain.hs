module Main where

import System.Environment

import Ast (entryFunctionName)
import Interpreter (run)
import Parser (parseFileM)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "error: no input files"
    (fileName : _) -> parseFileM fileName (run entryFunctionName)
