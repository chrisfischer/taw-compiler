module Parser where

import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec
import Control.Monad
import Ast

import System.Directory (doesDirectoryExist, getDirectoryContents)


-----------------------------
-- UTILS --------------------
-----------------------------

-- Printing / Parsing Utilities

printFile :: String -> IO ()
printFile fname =
  do s <- readFile fname
     print s

parseString :: String -> IO ()
parseString = parseTest langParser

parseFile :: FilePath -> IO ()
parseFile name =
  do s <- readFile name
     parseString s 

parseAllFiles :: String -> IO ()
parseAllFiles dir =
  do names <- getDirectoryContents dir
     let properNames = filter (`notElem` [".", ".."]) names
     paths <- forM properNames $ \name -> parseFile (dir ++ "/" ++ name)
     return ()

parseTawFiles :: IO ()
parseTawFiles = parseAllFiles "./tawprogs"

-- Ast.Node data type utilities

-- | converts an a to an Ast.Node a by using the a for
--   the element and 0 as an arbitrary loc
node :: a -> Ast.Node a
node = (\x -> x 0) . Ast.Node

-- | converts an m of a to a list of Ast.Node a
nodeMap :: Monad m => m a -> m (Ast.Node a)
nodeMap = (node <$>)

-- | takes a constructor and its two arguments and returns a node
--   of the result of applying the arguments to the constructor
nodeWrap2 c x y = node (c x y)

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

-- TODO: >>> needs its own constructor
-- TODO: our Ast has mod
-- TODO: OAT had bitflip - do we want that?
expOperators = 
  [ [Prefix (reservedOp "-"   >> return (nodeMap   (Uop Neg )))          ]
  , [Prefix (reservedOp "!"   >> return (nodeMap   (Uop Lognot )))       ]
  , [Infix  (reservedOp "*"   >> return (nodeWrap2 (Bop Mul ))) AssocLeft,
     Infix  (reservedOp "/"   >> return (nodeWrap2 (Bop Div ))) AssocLeft]
  , [Infix  (reservedOp "+"   >> return (nodeWrap2 (Bop Add ))) AssocLeft,
     Infix  (reservedOp "-"   >> return (nodeWrap2 (Bop Sub ))) AssocLeft]
  , [Infix  (reservedOp "<<"  >> return (nodeWrap2 (Bop Shl ))) AssocLeft,
     Infix  (reservedOp ">>"  >> return (nodeWrap2 (Bop Shr ))) AssocLeft,
     Infix  (reservedOp ">>>" >> return (nodeWrap2 (Bop Shr ))) AssocLeft]
  , [Infix  (reservedOp "<"   >> return (nodeWrap2 (Bop Lt  ))) AssocLeft,
     Infix  (reservedOp "<="  >> return (nodeWrap2 (Bop Lte ))) AssocLeft,
     Infix  (reservedOp ">"   >> return (nodeWrap2 (Bop Gt  ))) AssocLeft, 
     Infix  (reservedOp ">"   >> return (nodeWrap2 (Bop Gte ))) AssocLeft]
  , [Infix  (reservedOp "=="  >> return (nodeWrap2 (Bop Eq  ))) AssocLeft,
     Infix  (reservedOp "!="  >> return (nodeWrap2 (Bop Neq ))) AssocLeft]
  , [Infix  (reservedOp "&"   >> return (nodeWrap2 (Bop And ))) AssocLeft]
  , [Infix  (reservedOp "|"   >> return (nodeWrap2 (Bop Or  ))) AssocLeft]
  , [Infix  (reservedOp "[&]" >> return (nodeWrap2 (Bop IAnd))) AssocLeft]
  , [Infix  (reservedOp "[|]" >> return (nodeWrap2 (Bop IOr ))) AssocLeft]
  ]

exp :: Parser (Node Exp)
exp = buildExpressionParser expOperators expTerm

expTerm =   parens Parser.exp
        <|> nodeMap idExp
        <|> nodeMap boolExp
        <|> nodeMap intExp
        <|> nodeMap callExp


-----------------------------
-- EXPRESSIONS --------------
-----------------------------

boolExp :: Parser Exp
boolExp =   (reserved "true"  >> return (Ast.CBool True))
        <|> (reserved "false" >> return (Ast.CBool False))

intExp :: Parser Exp
intExp = Ast.CInt . fromInteger <$> integer

idExp :: Parser Exp
idExp = Ast.Id <$> identifier

callExp :: Parser Exp
callExp =
  do idEx <- Parser.exp
     argExs <- sepBy1 Parser.exp comma
     return $ Ast.Call idEx argExs


-----------------------------
-- DECLARATIONS -------------
-----------------------------

langParser :: Parser Prog
langParser = whiteSpace >> sequenceOfDecl

sequenceOfDecl :: Parser [Decl]
sequenceOfDecl = many decl

decl :: Parser Decl
decl = Ast.Gfdecl . node <$> fdecl

fdecl :: Parser Fdecl
fdecl = 
  do rty <- Parser.retty
     fname <- identifier
     as <- parens Parser.args
     b <- braces block
     return $ Ast.Fdecl rty fname as b

-- fdecl helper parsers
args :: Parser [(Ty, Id)]
args = sepBy1 arg comma

arg :: Parser (Ty, Id)
arg = do t <- ty
         id <- identifier
         return (t, id) 


-----------------------------
-- TYPES --------------------
-----------------------------

ty :: Parser Ty
ty =   (reserved "int"  >> return Ast.TInt)
   <|> (reserved "bool" >> return Ast.TBool)

retty :: Parser Retty
retty = RetVal <$> ty
-- TODO: account for void returns (here and AST)


-----------------------------
-- STATEMENTS ---------------
-----------------------------

block :: Parser Block
block = sepBy1 (Ast.Node <$> stmt) semi >>= \l -> return $ (\x -> x 0) <$> l
-- TODO: clean this up

stmt :: Parser Stmt
stmt =   retStmt
     <|> declStmt
     <|> assnStmt
--     <|> scallStmt
     <|> ifStmt
     <|> forStmt
     <|> whileStmt

assnStmt :: Parser Stmt
assnStmt = do lhs <- identifier
              reservedOp "="
              rhs <- Parser.exp
              semi
              return $ Ast.Assn (node $ Ast.Id lhs) rhs

declStmt :: Parser Stmt
declStmt = Ast.Decl <$> vdecl

vdecl :: Parser Ast.Vdecl
vdecl =
  do reserved "var"
     lhs <- identifier
     reservedOp "="
     rhs <- Parser.exp
     return $ Ast.Vdecl lhs rhs 

retStmt :: Parser Stmt
retStmt = reserved "return" >> Ast.Ret <$> Parser.exp <* semi

ifStmt :: Parser Stmt
ifStmt = 
  do reserved "if"
     condExp <- parens Parser.exp
     ifBlock <- braces block
     reserved "else"
     elseBlock <- braces block
     return $ Ast.If condExp ifBlock elseBlock

forStmt :: Parser Stmt
forStmt =
  do reserved "for"
     char '('
     vars  <- sepBy1 vdecl comma
     char ';'
     cond  <- Just <$> Parser.exp
     char ';'
     s     <- Just . node <$> Parser.stmt
     char ')'
     blck  <- braces block
     return $ Ast.For vars cond s blck
-- TODO account for missing forms (i.e. Nothing)

whileStmt :: Parser Stmt
whileStmt = 
  do reserved "while"
     condExp <- parens Parser.exp
     blck    <- braces block
     return $ Ast.While condExp blck

