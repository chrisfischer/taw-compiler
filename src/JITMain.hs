{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Control.Monad.Trans
import System.IO
import System.Environment
import System.Console.Haskeline
import Text.ParserCombinators.Parsec
import qualified Data.ByteString.Char8 as BS8

import qualified LLVM.AST as AST
import qualified LLVM.AST.Global as G
import qualified LLVM.Context as C
import qualified LLVM.Module as M

import JIT
import Frontend
import qualified LLVMGen as L
import qualified Ast as T
import qualified Parser as P

initModule :: AST.Module
initModule = L.emptyModule "module"

parseString :: String -> Either ParseError T.Prog
parseString s = parse P.langParser "" s

parseStmt :: String -> Either ParseError T.Stmt
parseStmt s = parse P.stmt "" s

parseExpr :: String -> Either ParseError T.Exp
parseExpr s = fmap (\(T.Node e _) -> e) (parse P.exp "" s)

data LoopContext
  = LoopContext {
      -- Accumulated LLVM module
      llmod :: AST.Module
      -- Accumulated main statements
      , mainStmts :: [T.Stmt]
      -- Functions
    , fs :: [T.Decl]
    , verbose :: Bool
    } deriving Show

emptyLoopContext :: LoopContext
emptyLoopContext = LoopContext initModule [] [] False


-- liftError :: ExceptT String IO a -> IO a
-- liftError = runExceptT >=> either fail return

-- Helper that creates a main function from the list of statements
mainFromStmts :: [T.Stmt] -> T.Retty -> T.Prog
mainFromStmts ss retty = [main]
  where
    main = T.Gfdecl (T.noLoc $ T.Fdecl retty "main" [] ss')
    ss' = map T.noLoc ss

removeDefinition :: String -> AST.Module -> AST.Module
removeDefinition id m =
  m { AST.moduleDefinitions = filter include (AST.moduleDefinitions m) }
  where
    include :: AST.Definition -> Bool
    include (AST.GlobalDefinition g) = G.name g /= (AST.Name $ idToShortBS id)
    include _ = True

nameFromDecl :: T.Decl -> String
nameFromDecl (T.Gfdecl (T.Node (T.Fdecl _ id _ _) _)) = id
nameFromDecl (T.Gfext (T.Node (T.Fext _ id _) _)) = id

removeDefinitionProg :: String -> T.Prog -> T.Prog
removeDefinitionProg id p = filter ((/= id) . nameFromDecl) p

jitProg :: LoopContext -> T.Prog -> LoopContext
jitProg ctxt p = ctxt { llmod = newast, fs = p ++ noDefsProg }
  where
    noDefs =
      foldr (\d acc -> removeDefinition (nameFromDecl d) acc) (llmod ctxt) p
    noDefsProg =
      foldr (\d acc -> removeDefinitionProg (nameFromDecl d) acc) (fs ctxt) p
    newast  = L.runLLVM noDefs $ cmpProg p

jitStmt :: LoopContext -> T.Stmt -> LoopContext
jitStmt ctxt s = ctxt { llmod = newAst, mainStmts = newSS }
  where
    newSS = (mainStmts ctxt) ++ [s]
    noMain = removeDefinition "main" $ llmod ctxt
    newAst = L.runLLVM noMain $
      cmpProg $ (fs ctxt) ++ mainFromStmts (newSS ++ [T.Ret Nothing]) T.RetVoid

jitExpr :: LoopContext -> T.Exp -> IO LoopContext
jitExpr ctxt e = do
  res <- runJIT newAst "main" (verbose ctxt)
  case res of
    Left err -> print err >> return ctxt
    Right val -> print val >> return ctxt { llmod = newAst }
  where
    newSS = (mainStmts ctxt) ++ [T.Ret $ Just $ T.noLoc e]
    noMain = removeDefinition "main" $ llmod ctxt
    newAst = L.runLLVM noMain $
      cmpProg $ (fs ctxt) ++ mainFromStmts newSS (T.RetVal T.TInt)

process :: LoopContext -> String -> IO LoopContext
process ctxt source = do
  case parseStmt source of
    Right stmt -> return $ jitStmt ctxt stmt
    Left err -> do
      case parseExpr source of
        Right e -> jitExpr ctxt e
        Left err -> do
          case parseString source of
            Right p -> return $ jitProg ctxt p
            Left err -> print err >> return ctxt

repl :: Bool -> IO ()
repl v = runInputT defaultSettings $ loop $ emptyLoopContext { verbose = v }
  where
  loop ctxt = do
    minput <- getInputLine "> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just "" -> loop ctxt
      Just input -> do
        ctxt' <- liftIO $ process ctxt input
        loop ctxt'

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-v"] -> repl True
    []     -> repl False