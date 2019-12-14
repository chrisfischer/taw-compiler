module Parser where

import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Control.Monad

import System.Directory (doesDirectoryExist, getDirectoryContents)

import Ast

-----------------------------
-- UTILS --------------------
-----------------------------

-- Printing / Parsing Utils

printFile :: String -> IO ()
printFile fname = do
  s <- readFile fname
  print s

parseString :: String -> String -> IO Prog
parseString s filePath =
  case parse langParser filePath s of
    Left e -> error $ show e
    Right p -> return p

parseFile :: FilePath -> IO Prog
parseFile name = do
  s <- readFile name
  parseString s name

parseFileTest :: FilePath -> IO ()
parseFileTest name = do
  s <- readFile name
  parseTest langParser s

parseFileD :: FilePath -> IO ()
parseFileD name = do
  s <- readFile $ "./tawprogs/" ++ name
  parseTest (parserTraced "langParser" langParser) s

parseAllFiles :: String -> IO ()
parseAllFiles dir = do
  names <- getDirectoryContents dir
  let properNames = filter (`notElem` [".", ".."]) names
  forM_ properNames $ \name ->
    print name >> parseFile (dir ++ "/" ++ name)

parseTawFiles :: IO ()
parseTawFiles = parseAllFiles "./tawprogs"

parseTawFile :: String -> IO Prog
parseTawFile name = parseFile $ "./tawprogs/" ++ name

-- AST Utils

-- | Converts an m of a to an m of Ast.Node a
nodeMap :: Monad m => m a -> m (Ast.Node a)
nodeMap = (noLoc <$>)

-- | Takes a constructor and its two arguments and returns a node
-- of the result of applying the arguments to the constructor
nodeWrap c x y = noLoc (c x y)

-----------------------------
-- PARSEC DEFS --------------
-----------------------------
-- modified from:
-- https://wiki.haskell.org/Parsing_a_simple_imperative_language

languageDef =
          emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "if"
                                     , "else"
                                     , "while"
                                     , "for"
                                     , "true"
                                     , "false"
                                     , "void"
                                     , "var"
                                     , "int"
                                     , "bool"
                                     ]
           , Token.reservedOpNames = ["+", "-", "*", "/", "="
                                     , "<", ">", "&&", "||", "!"
                                     , "==", "!=", ">>", "<<"
                                     , ">>>", "&", "|", "~"
                                     ]
           }


lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
braces     = Token.braces     lexer
integer    = Token.integer    lexer
semi       = Token.semi       lexer
comma      = Token.comma      lexer
whiteSpace = Token.whiteSpace lexer
openParen  = Token.symbol     lexer "("
closeParen = Token.symbol     lexer ")"
arrow      = Token.symbol     lexer "->"

-- TODO: >>> needs its own constructor
-- TODO: our Ast has mod
-- TODO: OAT had bitflip - do we want that?
expOperators =
  [ [Prefix (reservedOp "-"   >> return (nodeMap   (Uop Neg    )))       ]
  , [Prefix (reservedOp "!"   >> return (nodeMap   (Uop Lognot )))       ]
  , [Infix  (reservedOp "*"   >> return (nodeWrap (Bop Mul ))) AssocLeft,
     Infix  (reservedOp "/"   >> return (nodeWrap (Bop Div ))) AssocLeft]
  , [Infix  (reservedOp "+"   >> return (nodeWrap (Bop Add ))) AssocLeft,
     Infix  (reservedOp "-"   >> return (nodeWrap (Bop Sub ))) AssocLeft]
  , [Infix  (reservedOp "<<"  >> return (nodeWrap (Bop Shl ))) AssocLeft,
     Infix  (reservedOp ">>"  >> return (nodeWrap (Bop Shr ))) AssocLeft,
     Infix  (reservedOp ">>>" >> return (nodeWrap (Bop Shr ))) AssocLeft,
     Infix  (reservedOp "%"   >> return (nodeWrap (Bop Mod ))) AssocLeft]
  , [Infix  (reservedOp "<"   >> return (nodeWrap (Bop Lt  ))) AssocLeft,
     Infix  (reservedOp "<="  >> return (nodeWrap (Bop Lte ))) AssocLeft,
     Infix  (reservedOp ">"   >> return (nodeWrap (Bop Gt  ))) AssocLeft,
     Infix  (reservedOp ">="  >> return (nodeWrap (Bop Gte ))) AssocLeft]
  , [Infix  (reservedOp "=="  >> return (nodeWrap (Bop Eq  ))) AssocLeft,
     Infix  (reservedOp "!="  >> return (nodeWrap (Bop Neq ))) AssocLeft]
  , [Infix  (reservedOp "&&"  >> return (nodeWrap (Bop And ))) AssocLeft]
  , [Infix  (reservedOp "||"  >> return (nodeWrap (Bop Or  ))) AssocLeft]
  , [Infix  (reservedOp "&"   >> return (nodeWrap (Bop IAnd))) AssocLeft]
  , [Infix  (reservedOp "|"   >> return (nodeWrap (Bop IOr ))) AssocLeft]
  ]

exp :: Parser (Node Exp)
exp = buildExpressionParser expOperators expTerm

expTerm = parens Parser.exp
      <|> nodeMap boolExp
      <|> nodeMap intExp
      <|> nodeMap (try callExp)
      <|> nodeMap idExp


-----------------------------
-- EXPRESSIONS --------------
-----------------------------

boolExp :: Parser Exp
boolExp = (reserved "true"  >> return (Ast.CBool True))
      <|> (reserved "false" >> return (Ast.CBool False))

intExp :: Parser Exp
intExp = Ast.CInt . fromInteger <$> integer

idExp :: Parser Exp
idExp = Ast.Id <$> identifier

callExp :: Parser Exp
callExp = do
  idEx <- idExp
  argExs <- parens $ sepEndBy Parser.exp comma
  return $ Ast.Call (noLoc idEx) argExs


-----------------------------
-- DECLARATIONS -------------
-----------------------------

langParser :: Parser Prog
langParser = whiteSpace >> sequenceOfDecl

sequenceOfDecl :: Parser [Decl]
sequenceOfDecl = many decl

decl :: Parser Decl
decl = Ast.Gfdecl . noLoc <$> (try fdecl)
   <|> Ast.Gfext  . noLoc <$> fext

fdecl :: Parser Fdecl
fdecl = do
  rty <- Parser.retty
  fname <- identifier
  as <- parens Parser.args
  b <- braces block
  return $ Ast.Fdecl rty fname as b

fext :: Parser Fext
fext = do
  rty <- Parser.retty
  fname <- identifier
  as <- parens Parser.args
  semi
  return $ Ast.Fext rty fname as

vdecl :: Parser Vdecl
vdecl = do
  reserved "var"
  lhs <- identifier
  reservedOp "="
  rhs <- Parser.exp
  return $ Ast.Vdecl lhs rhs

-- fdecl helper parsers
args :: Parser [(Ty, Id)]
args = sepEndBy arg comma

arg :: Parser (Ty, Id)
arg = do t <- ty
         id <- identifier
         return (t, id)


-----------------------------
-- TYPES --------------------
-----------------------------

ty :: Parser Ty
ty =  (reserved "int"  >> return Ast.TInt)
  <|> (reserved "bool" >> return Ast.TBool)
  <|> TRef <$> rty

rty :: Parser Rty
rty = do
  tys <- parens $ sepEndBy ty comma
  arrow
  r <- Parser.retty
  return $ RFun tys r

retty :: Parser Retty
retty = (reserved "void" >> return Ast.RetVoid)
    <|> RetVal <$> ty

-----------------------------
-- STATEMENTS ---------------
-----------------------------

block :: Parser Block
block = many (noLoc <$> stmt)

stmt :: Parser Stmt
stmt = retStmt
   <|> (try scallStmt)
   <|> declStmt
   <|> assnStmt
   <|> ifStmt
   <|> forStmt
   <|> whileStmt

assnStmt :: Parser Stmt
assnStmt = do
  idEx <- idExp
  reservedOp "="
  rhs <- Parser.exp
  semi
  return $ Ast.Assn (noLoc idEx) rhs

scallStmt :: Parser Stmt
scallStmt = do
  idEx <- idExp
  argExs <- parens $ sepEndBy Parser.exp comma
  semi
  return $ Ast.SCall (noLoc idEx) argExs

declStmt :: Parser Stmt
declStmt = Ast.Decl <$> vdecl <* semi

retStmt :: Parser Stmt
retStmt = (try $ reserved "return" >> Ast.Ret . Just <$> Parser.exp <* semi)
      <|> (reserved "return" >> return (Ast.Ret Nothing) <* semi)

ifStmt :: Parser Stmt
ifStmt = (try ifWithElse) <|> ifNoElse

ifWithElse :: Parser Stmt
ifWithElse = do
  reserved "if"
  condExp <- parens Parser.exp
  ifBlock <- braces block
  reserved "else"
  elseBlock <- Just <$> braces block
  return $ Ast.If condExp ifBlock elseBlock

ifNoElse :: Parser Stmt
ifNoElse = do
  reserved "if"
  condExp <- parens Parser.exp
  ifBlock <- braces block
  return $ Ast.If condExp ifBlock (Nothing)

forStmt :: Parser Stmt
forStmt = do
  reserved "for"
  openParen
  vars  <- sepEndBy vdecl comma
  semi
  cond  <- Parser.exp
  semi
  s     <- noLoc <$> Parser.stmt
  closeParen
  blck  <- braces block
  return $ Ast.For vars (Just cond) (Just s) blck
-- TODO account for missing forms (i.e. Nothing)

whileStmt :: Parser Stmt
whileStmt = do
  reserved "while"
  condExp <- parens Parser.exp
  blck    <- braces block
  return $ Ast.While condExp blck

