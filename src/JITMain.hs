{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Except

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

parseProg :: String -> Either ParseError T.Prog
parseProg = parse P.langParser ""

parseStmt :: String -> Either ParseError T.Stmt
parseStmt = parse P.stmt ""

parseExpr :: String -> Either ParseError T.Exp
parseExpr s = T.elt <$> parse P.exp "" s

data LoopContext
  = LoopContext {
      -- Accumulated LLVM module
      llmod :: AST.Module
      -- Accumulated main statements
    , mainStmts :: [T.Stmt]
      -- Declared functions, needed for the types when compiling new statements
    , fs :: [T.Decl]
    , verbose :: Bool
    } deriving Show

-- | Empty context, defaults to verbose = False
emptyLoopContext :: LoopContext
emptyLoopContext = LoopContext (L.emptyModule "module") [] [] False

-- | Append a statement to the end of the main function's block
addMainStmt :: (MonadState LoopContext) m => T.Stmt -> m ()
addMainStmt s = do
  ctxt <- get
  put ctxt { mainStmts = mainStmts ctxt ++ [s] }

-- | Remove any LLVM function with the given name
removeDefinitionLL :: (MonadState LoopContext) m => String -> m ()
removeDefinitionLL id = do
  m <- gets llmod
  let m' = m {
    AST.moduleDefinitions = filter include (AST.moduleDefinitions m) }
  modify $ \s -> s { llmod = m' }
  where
    -- | Only keep names that are not equal to the one given
    include :: AST.Definition -> Bool
    include (AST.GlobalDefinition g) = G.name g /= (AST.Name $ idToShortBS id)
    include _ = True

-- | Remove any Taw function with the given name
removeDefinitionProg :: (MonadState LoopContext) m => String -> m ()
removeDefinitionProg id = do
  funs <- gets fs
  modify $ \s -> s { fs = filter ((/= id) . T.nameFromDecl) funs }

-- | Remove a function from the current context with the given name
removeDefinition :: (MonadState LoopContext) m => String -> m ()
removeDefinition id = removeDefinitionLL id >> removeDefinitionProg id

-- | Helper that creates a main function from the list of statements
mainFromStmts :: (MonadState LoopContext) m => Maybe T.Exp -> T.Retty ->
                 m T.Decl
mainFromStmts retExp retty = do
  ss <- gets mainStmts
  let retStmt = T.Ret (fmap T.noLoc retExp)
      block = map T.noLoc $ ss ++ [retStmt]
  return $ T.Gfdecl (T.noLoc $ T.Fdecl retty T.entryFunctionName [] block)

-- | Update the return expression and type of the main function
updateMainRet :: ((MonadState LoopContext) m, MonadError String m) =>
                 Maybe T.Exp -> T.Retty -> m ()
updateMainRet retExp retty = do
  removeDefinitionLL T.entryFunctionName
  main' <- mainFromStmts retExp retty
  s <- get
  case cmpProgWithModule (llmod s) $ T.Prog (fs s ++ [main']) of
    Left err -> throwError err
    Right ll' -> modify $ \s -> s { llmod = ll' }

-- | Remove any previous definitions of the functions and compile the given
-- program and make functions available for future statements and expressions
jitProg :: ((MonadState LoopContext) m, MonadIO m, MonadError String m) =>
           T.Prog -> m ()
jitProg p@(T.Prog prog) =
  if declaresMain prog then throwError "cannot declare main"
  else do
    s <- get
    mapM_ (removeDefinition . T.nameFromDecl) prog
    case cmpProgWithModule (llmod s) p of
      Left err -> throwError err
      Right ll -> modify $ \s -> s { llmod = ll, fs = prog ++ fs s }
  where
    -- | Ensure that main was not declared
    declaresMain :: [T.Decl] -> Bool
    declaresMain = any $ (== T.entryFunctionName) . T.nameFromDecl

-- | Compile the given statement and make any declarations available
-- for future statements and expressions
jitStmt :: ((MonadState LoopContext) m, MonadError String m) => T.Stmt -> m ()
jitStmt s = do
  addMainStmt s
  updateMainRet Nothing T.RetVoid

-- | Compile the given expression and prints the result or error
jitExpr :: ((MonadState LoopContext) m, MonadIO m, MonadError String m) => T.Exp -> m ()
jitExpr e = do
  updateMainRet (Just e) (T.RetVal T.TInt)
  ctxt <- get
  res <- liftIO $
    runJIT (llmod ctxt) (idToShortBS T.entryFunctionName) (verbose ctxt)
  case res of
    Left err -> throwError err
    Right val -> liftIO $ print val

-- | Process text input, could be a statement, an expression, or a program
-- definition
process :: ((MonadState LoopContext) m, MonadIO m, MonadError String m) =>
           String -> m ()
process source =
  let m = case parseStmt source of
            Right stmt -> jitStmt stmt
            Left err ->
              case parseExpr source of
                Right e -> jitExpr e
                Left err ->
                  case parseProg source of
                    Right p -> jitProg p
                    Left err -> liftIO $ print err in
  catchError m $ \err -> liftIO $ putStrLn $ "error: " ++ err

-- | Shell loop
loop :: ExceptT String (StateT LoopContext (InputT IO)) ()
loop = do
  removeDefinition T.entryFunctionName
  minput <- lift $ lift $ getInputLine "> "
  case minput of
    Nothing -> return ()
    Just "" -> loop
    Just input -> do
      process input
      loop

-- | Calls the shell loop, if a filename is given, will try to parse and
-- compile it
repl :: Bool -> Maybe String -> IO ()
repl v Nothing =
  void $ runInputT defaultSettings $ evalStateT (runExceptT loop)
    emptyLoopContext { verbose = v }
repl v (Just fileName) =
  P.parseFileM fileName $ \p@(T.Prog prog) ->
    cmpProgM fileName p $ \ll ->
      void $ runInputT defaultSettings $ evalStateT (runExceptT loop)
          emptyLoopContext { verbose = v, llmod = ll, fs = prog }

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("-v" : fileName : _) -> repl True  $ Just fileName
    ["-v"]                -> repl True  Nothing
    [fileName]            -> repl False $ Just fileName
    []                    -> repl False Nothing