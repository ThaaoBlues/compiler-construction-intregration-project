module MyParser
    ( parseMyLang
    ) where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language (haskellStyle)
import Text.Parsec.Expr (Assoc(..), buildExpressionParser, Operator(..))
import Text.Parsec.Combinator (chainl1)

-- Define the lexer
lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser haskellStyle
  { Tok.commentLine = "//"
  , Tok.commentStart = "/*"
  , Tok.commentEnd = "*/"
  , Tok.identStart = letter <|> char '_'
  , Tok.identLetter = alphaNum <|> char '_'
  , Tok.reservedNames = ["entero", "booleana", "array", "si", "durante", "imprimir", "hilo", "empezamos", "esperamos", "global", "esclusa", "liberar", "obtener", "iniciamos", "cerramos", "verdad", "mentira"]
  , Tok.reservedOpNames = ["+", "-", "*", "==", "!=", "<", "<=", ">", ">=", "Y", "O", "~:(", "=", ":)", ",", "[", "]", "¡", "!", "{", "}"]
  }

-- Helper parsers
reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

integer :: Parser Integer
integer = Tok.integer lexer

symbol :: String -> Parser String
symbol = Tok.symbol lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

braces :: Parser a -> Parser a
braces = Tok.braces lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

-- AST definition
data Type = Entero | Booleana | Array
          deriving (Show, Eq)

data Op = Mul 
    | Add 
    | Sub 
    | Inv 
    | Not 
    | Or 
    | And
    | Lt
    | Leq
    | Gt
    | Geq
    | Eq
    | Neq
        deriving (Show,Eq)

data Expr = IntLit Integer
          | BoolLit Bool
          | Var String
          | ArrayLit [Expr]
          | BinOp Op Expr Expr
          | UnOp Op Expr
          | Paren Expr
          deriving (Show)

data Stmt = Declaration Type String
          | Assignment String Expr
          | If Expr [Stmt]
          | While Expr [Stmt]
          | Print Expr
          | ThreadCreate [Stmt]
          | ThreadJoin
          | StartThread String
          | GlobalDecl String
          | LockCreate String
          | LockFree String
          | LockGet String
          | ScopeBlock [Stmt]
          deriving (Show)

-- Parser for type identifiers
typeIdentifier :: Parser Type
typeIdentifier = (Entero <$ reserved "entero")
            <|> (Booleana <$ reserved "booleana")
            <|> (Array <$ reserved "array")

-- Parser for primary expressions
primaryExpr :: Parser Expr
primaryExpr = try (IntLit <$> integer)
          <|> try (BoolLit True <$ reserved "verdad")
          <|> try (BoolLit False <$ reserved "mentira")
          <|> try (Var <$> identifier)
          <|> try arrayLiteral
          <|> (Paren <$> (symbol "¡" *> expr) <* symbol "!")

arrayLiteral :: Parser Expr
arrayLiteral = (ArrayLit <$ symbol "[") <*> (commaSep expr <* symbol "]")

-- Parser for unary expressions
unaryExpr :: Parser Expr
unaryExpr = (UnOp Not <$> (reservedOp "~:(" *> unaryExpr)) -- NOT
            <|> (UnOp Inv <$> (reservedOp "-" *> unaryExpr))-- Int INVERSION
            <|> primaryExpr

-- Parser for multiplicative expressions
multExpr :: Parser Expr
multExpr = chainl1 unaryExpr (BinOp Mul <$ reservedOp "*")

-- Parser for additive expressions
addExpr :: Parser Expr
addExpr = chainl1 multExpr $ (BinOp Add <$ reservedOp "+") <|> (BinOp Sub <$ reservedOp "-")

-- Parser for relational expressions
relExpr :: Parser Expr
relExpr = do
  e1 <- addExpr
  (do
      op <- (Lt  <$ reservedOp "<")
          <|> (Leq <$ reservedOp "<=")
          <|> (Gt  <$ reservedOp ">")
          <|> (Geq <$ reservedOp ">=")
      BinOp op e1 <$> addExpr
   ) <|> return e1



-- Parser for equality expressions
equalityExpr :: Parser Expr
equalityExpr = do
  e1 <- relExpr
  (do op <- ( Eq <$ reservedOp "==") <|> ( Neq <$ reservedOp "!=")
      BinOp op e1 <$> relExpr
   ) <|> return e1

-- Parser for AND expressions
andExpr :: Parser Expr
andExpr = chainl1 equalityExpr (BinOp And <$ reservedOp "Y")

-- Parser for OR expressions
orExpr :: Parser Expr
orExpr = chainl1 andExpr (BinOp Or <$ reservedOp "O")

expr :: Parser Expr
expr = orExpr

-- Helper parsers for scopes
beginScope :: Parser ()
beginScope = reserved "iniciamos" <|> () <$ symbol "{"

endScope :: Parser ()
endScope = reserved "cerramos" <|> () <$ symbol "}"

scopeBlock' :: Parser [Stmt]
scopeBlock' = do
  beginScope
  blockContent <- many statement
  endScope
  return blockContent

scopeBlock :: Parser Stmt
scopeBlock = do
  beginScope
  blockContent <- many statement
  endScope
  symbol ":)"
  return $ ScopeBlock blockContent

-- Parser for statements
declaration :: Parser Stmt
declaration = do
  typeId <- typeIdentifier
  varName <- identifier
  symbol ":)"
  return $ Declaration typeId varName

assignment :: Parser Stmt
assignment = do
  varName <- identifier
  symbol "="
  exprVal <- expr
  symbol ":)"
  return $ Assignment varName exprVal

ifStatement :: Parser Stmt
ifStatement = do
  reserved "si"
  cond <- expr
  -- symbol ":)"
  If cond <$> scopeBlock'

whileStatement :: Parser Stmt
whileStatement = do
  reserved "durante"
  cond <- expr
  -- symbol ":)"
  While cond <$> scopeBlock'

printStatement :: Parser Stmt
printStatement = do
  reserved "imprimir"
  symbol "¡"
  exprVal <- expr
  symbol "!:)"
  return $ Print exprVal

threadCreate :: Parser Stmt
threadCreate = do
  reserved "hilo"
  blockContent <- scopeBlock'
  symbol ":)"
  return $ ThreadCreate blockContent

threadJoin :: Parser Stmt
threadJoin = do
  reserved "esperamos"
  symbol ":)"
  return $ ThreadJoin

startThread :: Parser Stmt
startThread = do
  reserved "empezamos"
  threadName <- identifier
  symbol ":)"
  return $ StartThread threadName

globalDecl :: Parser Stmt
globalDecl = do
  reserved "global"
  varName <- identifier
  symbol ":)"
  return $ GlobalDecl varName

lockCreate :: Parser Stmt
lockCreate = do
  reserved "esclusa"
  lockName <- identifier
  symbol ":)"
  return $ LockCreate lockName

lockFree :: Parser Stmt
lockFree = do
  reserved "liberar"
  lockName <- identifier
  symbol ":)"
  return $ LockFree lockName

lockGet :: Parser Stmt
lockGet = do
  reserved "obtener"
  lockName <- identifier
  symbol ":)"
  return $ LockGet lockName

blockParser :: Parser [Stmt]
blockParser = many statement

statement :: Parser Stmt
statement = try declaration
        <|> try assignment
        <|> try ifStatement
        <|> try whileStatement
        <|> try printStatement
        <|> try threadCreate
        <|> try threadJoin
        <|> try startThread
        <|> try globalDecl
        <|> try lockCreate
        <|> try lockFree
        <|> try lockGet
        <|> scopeBlock

program :: Parser [Stmt]
program = many statement <* eof

-- Helper function to parse a program
parseMyLang :: String -> Either ParseError [Stmt]
parseMyLang = parse program ""



