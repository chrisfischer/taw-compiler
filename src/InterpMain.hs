module Main where

import System.FilePath
import System.Environment

import Interpreter (run)
import Parser (parseFile)

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  p <- parseFile fileName
  run "main" p


main2 fileName = do
  p <- parseFile fileName
  run "main" p
