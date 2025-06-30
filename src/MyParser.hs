module MyParser
    ( parseMyLang, stackChecking,fillSymbolTable, Type(..), Stmt(..), Expr(..),Op(..)
    ) where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language (haskellStyle)
import Text.Parsec.Expr (Assoc(..), buildExpressionParser, Operator(..))
import Text.Parsec.Combinator (chainl1)
import Data.Type.Coercion (sym)

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
data Type = Entero | Booleana | Array Type| Lock | Global Type |CompilationError String
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

data Expr = 
            ArrayAccess String Expr
          | IntLit Integer
          | BoolLit Bool
          | Var String
          | ArrayLit [Expr]
          | BinOp Op Expr Expr
          | UnOp Op Expr
          | Paren Expr
          deriving (Show,Eq)

data Stmt = Declaration Type String
          | Assignment String Expr
          | If Expr [Stmt] [Stmt] -- The if contains the else body
          | While Expr [Stmt]
          | Print Expr
          | ThreadCreate [Stmt]
          | ThreadJoin
          | StartThread String
          | LockCreate String
          | LockFree String
          | LockGet String
          | ScopeBlock [Stmt]
          deriving (Show,Eq)

-- Parser for type identifiers
typeIdentifier :: Parser Type
typeIdentifier =
              (Global <$ reserved "global") <*> typeIdentifier
            <|> (Entero <$ reserved "entero")
            <|> (Booleana <$ reserved "booleana")
            <|> ( (Array <$ reserved "array") <*> typeIdentifier) -- ex : array entero = [1,2,3]:)

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
expr = try accessArray <|> orExpr 

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
  --symbol ":)"
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
  If cond <$> scopeBlock' <*> option [] (reserved "sino" *> scopeBlock')

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
  symbol "!"
  symbol ":)"
  return $ Print exprVal

threadCreate :: Parser Stmt
threadCreate = do
  reserved "hilo"
  -- symbol ":)"
  ThreadCreate <$> scopeBlock'

threadJoin :: Parser Stmt
threadJoin = do
  reserved "esperamos"
  symbol ":)"
  return ThreadJoin

accessArray :: Parser Expr
accessArray = do 
  id <- identifier
  symbol "["
  index <- expr
  symbol "]"

  return $ ArrayAccess id index

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
        <|> try lockCreate
        <|> try lockFree
        <|> try lockGet
        <|> scopeBlock

program :: Parser [Stmt]
program = many statement <* eof

-- Helper function to parse a program
parseMyLang :: String -> Either ParseError [Stmt]
parseMyLang = parse program ""

{-
================================================================

TYPE CHECKING

================================================================


go throught the AST generated by the parser and check for type errors

-}




type SymbolTable = [(String,Type)]
type STStack = [SymbolTable] -- to support scopes


-- manage symbol table stack and perform type checking at the same type
stackChecking:: [Stmt]->STStack->Bool
-- (:) operator appends at front of the array, 
-- exatcly what we want as we are going recursively deeper
stackChecking [] _ = True
stackChecking s@(x:xs) stack =
  -- create contextual stack
  let st = fillSymbolTable s:stack 
  in case x of

    -- the condition in a if must be a boolean
    (If e body1 body2) -> stackChecking body1 st && stackChecking body2 st && inferType e st == Booleana

    -- the condition in a while must be a boolean
    (While e body) -> stackChecking body st && inferType e st == Booleana

    -- assigned variable must be from the same type as the assigning expression
    (Assignment id e) -> inferType e st == 
      -- handling variable out of scope error
      case lookupStack st id of
        (Left s) -> CompilationError s
        (Right t) -> t

    -- check if locks objects exists
    (LockGet id) -> case lookupStack st id of
      (Left s) -> False
      (Right t) -> True
    
    (LockFree id) -> case lookupStack st id of
      (Left s) -> False
      (Right t) -> True

    -- go recursively deeper in the nested scope
    (ThreadCreate body) -> stackChecking body st
    (ScopeBlock body) -> stackChecking body st
  
    _ -> True -- other statements that we do not need to check
    -- print takes also an expression but we print every types

    && stackChecking xs stack

-- actually takes care of filling a symbol table on a specific depth
fillSymbolTable::[Stmt]->SymbolTable
fillSymbolTable [] = []
fillSymbolTable ((Declaration t id):xs) = (id,t) : fillSymbolTable xs 
fillSymbolTable ((LockCreate id):xs) = (id,Lock) : fillSymbolTable xs
fillSymbolTable (_:xs) = fillSymbolTable xs


-- data Expr = IntLit Integer
--           | BoolLit Bool
--           | Var String
--           | ArrayLit [Expr]
--           | BinOp Op Expr Expr
--           | UnOp Op Expr
--           | Paren Expr

lookupStack::STStack->String->Either String Type
lookupStack [] _ = Left "Variable out of scope"
lookupStack (t:ts) id = case lookupTable t id of
                          (Left s) -> lookupStack ts id
                          (Right t) -> Right t

lookupTable::SymbolTable->String->Either String Type
lookupTable [] tid = Left "Variable out of scope"
lookupTable [(id,vartype)] tid 
  | id == tid = Right vartype
  | otherwise = Left "Variable out of scope"

lookupTable ((id,vartype):xs) tid 
  | id == tid = Right vartype
  | otherwise = lookupTable xs tid

-- data Op = Mul 
--     | Add 
--     | Sub 
--     | Inv 
--     | Not 
--     | Or 
--     | And
--     | Lt
--     | Leq
--     | Gt
--     | Geq
--     | Eq
--     | Neq

allTypesEq::[Type]->Bool
allTypesEq (x:xs) = null (filter (\t-> t /= x) xs)

inferType:: Expr->STStack->Type
inferType (IntLit _) st = Entero
inferType (BoolLit _) st = Booleana
inferType (ArrayLit xs) st 
  | allTypesEq (map (flip inferType st) xs) = Array $ inferType (head xs) st
  | otherwise = CompilationError "Cannot create an array of non-homogeneous types"
inferType (Var id) st = case lookupStack st id of
  (Left s) -> CompilationError s
  (Right t) -> t
-- allows only integer multiplication
inferType (BinOp (Mul) e1 e2) st
  | t1 == Entero && t2 == Entero = Entero
  | otherwise = CompilationError "Cannot multiply two types other than Entero"
  where t1 = inferType e1 st
        t2 = inferType e2 st

inferType (BinOp (Add) e1 e2) st
  | t1 == Entero && t2 == Entero = Entero
  | otherwise = CompilationError "Cannot add two types other than Entero"
  where t1 = inferType e1 st
        t2 = inferType e2 st

inferType (BinOp (Sub) e1 e2) st
  | t1 == Entero && t2 == Entero = Entero
  | otherwise = CompilationError "Cannot subtract two types other than Entero"
  where t1 = inferType e1 st
        t2 = inferType e2 st

inferType (UnOp (Inv) e1) st
  | t1 == Entero = Entero
  | otherwise = CompilationError "Cannot Invert types other than Entero"
  where t1 = inferType e1 st

inferType (UnOp (Not) e1) st
  | t1 == Booleana = Booleana
  | otherwise = CompilationError "Cannot negate two types other than Booleana"
  where t1 = inferType e1 st


inferType (BinOp (Or) e1 e2) st
  | t1 == Booleana && t2 == Booleana = Booleana
  | otherwise = CompilationError "Cannot OR two types other than Booleana"
  where t1 = inferType e1 st
        t2 = inferType e2 st

inferType (BinOp (And) e1 e2) st
  | t1 == Booleana && t2 == Booleana = Booleana
  | otherwise = CompilationError "Cannot AND two types other than Booleana"
  where t1 = inferType e1 st
        t2 = inferType e2 st

inferType (BinOp (Eq) e1 e2) st
  | t1 == t2 = Booleana
  | otherwise = CompilationError "Cannot equate two differents types"
  where t1 = inferType e1 st
        t2 = inferType e2 st

inferType (BinOp (Neq) e1 e2) st
  | t1 == t2 = Booleana
  | otherwise = CompilationError "Cannot equate two differents types"
  where t1 = inferType e1 st
        t2 = inferType e2 st

-- the rest of operators are quantity comparators
inferType (BinOp (_) e1 e2) st
  | t1 == Entero && t2 == Entero = Entero
  | otherwise = CompilationError "Cannot compare two types other than Entero"
  where t1 = inferType e1 st
        t2 = inferType e2 st

inferType (ArrayAccess id e) st | (t1 == Entero) && 
      case lookupStack st id of
        (Left _) -> False
        (Right _) -> True
      
      = case lookupStack st id of
          (Left s) -> CompilationError s
          (Right a) -> a
      | otherwise = CompilationError ("Invalid access to array "++id)

      where t1 = inferType e st

-- parenthesised expression
inferType (Paren e) st = inferType e st