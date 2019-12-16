{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Data.Int

import Control.Monad
import Control.Monad.IO.Class

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Ast
import AstGen
import JIT (runJIT)
import Frontend (cmpProg, cmpProgM, idToShortBS)
import Interpreter (executeProg, ValueTy(VInt))
import PrettyAst

prop_interpCmp :: Prog -> Property
prop_interpCmp p = monadicIO $ do
  case cmpProg "module" p of
    Left err -> error err
    Right ll -> do
      let resInterp = executeProg p entryFunctionName
      resCmp <- liftIO $ runJIT ll (idToShortBS entryFunctionName) False
      case (resInterp, resCmp) of
        (Right (VInt v1), Right v2) -> assert $ v1 == fromIntegral v2
        (Left err, _) -> showError err
        (_, Left err) -> showError err
  where
    showError err = error $ show err

main :: IO ()
main = do
  -- p <- run'
  -- putStrLn $ renderProg p
  -- cmpProgM "wat" p $ \ll -> void $ runJIT ll (idToShortBS entryFunctionName) False
  quickCheckWith stdArgs { maxSuccess = 10 } prop_interpCmp
  -- return ()
