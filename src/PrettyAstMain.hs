module Main where

import System.FilePath
import System.Environment

import Ast (entryFunctionName)
import PrettyAst (renderProg)
import Parser (parseFile)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (fileName : _) -> do
      p <- parseFile fileName
      writeFile fileName $ renderProg p
    _ -> putStrLn "error: no input files"
