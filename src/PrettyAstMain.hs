module Main where

import System.Environment

import PrettyAst (renderProg)
import Parser (parseFileM)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "error: no input files"
    (fileName : _) -> parseFileM fileName ((writeFile fileName) . renderProg)
