module MyParser
    ( parseMyLang, stackChecking,fillSymbolTable,fillGlobalSymbolTable, Type(..), Stmt(..), Expr(..),Op(..)
    ) where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language (haskellStyle)
import Text.Parsec.Expr (Assoc(..), buildExpressionParser, Operator(..))
import Text.Parsec.Combinator (chainl1)
import Data.Type.Coercion (sym)

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

-- helper parsers
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

-- EDSL definition
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

-- parser for type identifiers
typeIdentifier :: Parser Type
typeIdentifier =
              (Global <$ reserved "global") <*> typeIdentifier
            <|> (Entero <$ reserved "entero")
            <|> (Booleana <$ reserved "booleana")
            <|> ( (Array <$ reserved "array") <*> typeIdentifier) -- ex : array entero = [1,2,3]:)

-- parser for primary expressions (directly hardcoded things)
primaryExpr :: Parser Expr
primaryExpr = try (IntLit <$> integer)
          <|> try (BoolLit True <$ reserved "verdad")
          <|> try (BoolLit False <$ reserved "mentira")
          <|> try (Var <$> identifier)
          <|> try arrayLiteral
          <|> (Paren <$> (symbol "¡" *> expr) <* symbol "!")

arrayLiteral :: Parser Expr
arrayLiteral = (ArrayLit <$ symbol "[") <*> (commaSep expr <* symbol "]")

-- parser for unary expressions
unaryExpr :: Parser Expr
unaryExpr = (UnOp Not <$> (reservedOp "~:(" *> unaryExpr)) -- NOT
            <|> (UnOp Inv <$> (reservedOp "-" *> unaryExpr))-- Int INVERSION
            <|> primaryExpr

-- parser for multiplicative expressions
multExpr :: Parser Expr
multExpr = chainl1 unaryExpr (BinOp Mul <$ reservedOp "*")

-- parser for additive expressions
addExpr :: Parser Expr
addExpr = chainl1 multExpr $ (BinOp Add <$ reservedOp "+") <|> (BinOp Sub <$ reservedOp "-")

-- parser for relational expressions
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



-- parser for equality expressions
equalityExpr :: Parser Expr
equalityExpr = do
  e1 <- relExpr
  (do op <- ( Eq <$ reservedOp "==") <|> ( Neq <$ reservedOp "!=")
      BinOp op e1 <$> relExpr
   ) <|> return e1

-- parser for AND expressions
andExpr :: Parser Expr
andExpr = chainl1 equalityExpr (BinOp And <$ reservedOp "Y")

-- parser for OR expressions
orExpr :: Parser Expr
orExpr = chainl1 andExpr (BinOp Or <$ reservedOp "O")

expr :: Parser Expr
expr = try accessArray <|> orExpr 

-- helper parsers for scopes
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

-- parser for statements
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

-- helper function to parse a program
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


-- called stackChecking because we make use of a stack to check types of local variables and expressions
stackChecking :: [Stmt] -> SymbolTable->STStack -> [String]
stackChecking [] _ _ = []
stackChecking (x:xs) gt stack =
  let st = fillSymbolTable (x:xs) : stack
  in case x of

    If e body1 body2 ->
      let errsCond = case inferType e gt st of
                        Booleana -> []
                        CompilationError msg -> [msg]
                        t -> ["If condition expected Booleana but got " ++ show t]
          errsBody1 = stackChecking body1 gt st
          errsBody2 = stackChecking body2 gt st
      in errsCond ++ errsBody1 ++ errsBody2 ++ stackChecking xs gt stack

    While e body ->
      let errsCond = case inferType e gt st of
                        Booleana -> []
                        CompilationError msg -> [msg]
                        t -> ["While condition expected Booleana but got " ++ show t]
          errsBody = stackChecking body gt st
      in errsCond ++ errsBody ++ stackChecking xs gt stack

    Assignment id e ->
        let exprType = inferType e gt st
                        

            errType = case exprType of
                        (CompilationError s ) -> [s]
                        _ -> []
            
            varType = case lookupStack st id of
              (Left s) -> case lookupTable gt id of
                            (Left s)-> CompilationError s
                            (Right t) -> t
              (Right t) -> t


            errVarLookup = case varType of
                        (CompilationError s ) -> [s]
                        _ -> []
            

            errAssign = case varType of
                  (CompilationError s) -> []
                  t1 -> case exprType of
                    (CompilationError s) -> []
                    t2 -> if t1 /= t2 
                            then ["Type mismatch in assignment to " ++ id ++
                                            ": variable is " ++ show t1 ++
                                            " but expression is " ++ show t2]
                            else []
                            
        in errAssign++ errType++errVarLookup ++ stackChecking xs gt stack

    LockGet id ->
      let errs = case lookupStack st id of
                   (Left s )-> case lookupTable gt id of
                                (Left s) -> [s]
                                (Right _) -> []
                   (Right _)  -> []
      in errs ++ stackChecking xs gt stack

    LockFree id ->
      let errs = case lookupStack st id of
                   (Left s) -> case lookupTable gt id of
                                  (Left s)-> [s]
                                  (Right _)->[]
                   (Right _)  -> []
      in errs ++ stackChecking xs gt stack

    ThreadCreate body ->
      -- new scope, no outer variables
      let newStack = [fillSymbolTable body]
      in stackChecking body gt newStack ++ stackChecking xs gt stack

    ScopeBlock body ->
      -- keep outer stack + new scope on top
      let newStack = fillSymbolTable body : stack
      in stackChecking body gt newStack ++ stackChecking xs gt stack

    _ -> stackChecking xs gt stack


-- actually takes care of filling a symbol table on a specific depth
fillSymbolTable::[Stmt]->SymbolTable
fillSymbolTable [] = []
-- skip global variables here, as we only fill local ones
fillSymbolTable ((Declaration (Global _) _):xs) = fillSymbolTable xs

fillSymbolTable ((Declaration t id):xs) = (id,t) : fillSymbolTable xs 
fillSymbolTable (_:xs) = fillSymbolTable xs

-- same but for global variables
fillGlobalSymbolTable::[Stmt]->SymbolTable
fillGlobalSymbolTable [] = []
fillGlobalSymbolTable ((Declaration (Global t) id):xs) = (id,t) : fillSymbolTable xs 
fillGlobalSymbolTable ((LockCreate id):xs) = (id,Lock) : fillGlobalSymbolTable xs
fillGlobalSymbolTable (_:xs) = fillGlobalSymbolTable xs


-- data Expr = IntLit Integer
--           | BoolLit Bool
--           | Var String
--           | ArrayLit [Expr]
--           | BinOp Op Expr Expr
--           | UnOp Op Expr
--           | Paren Expr

lookupStack::STStack->String->Either String Type
lookupStack [] id = Left ("Variable "++id++" out of scope")
lookupStack (t:ts) id = case lookupTable t id of
                          (Left s) -> lookupStack ts id
                          (Right t) -> Right t

lookupTable::SymbolTable->String->Either String Type
lookupTable [] tid = Left ("Variable "++tid++" out of scope")
lookupTable [(id,vartype)] tid 
  | id == tid = Right vartype
  | otherwise = Left ("Variable "++tid++" of wrong type or out of scope")

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

inferType:: Expr->SymbolTable->STStack->Type

inferType (IntLit _) _ st = Entero
inferType (BoolLit _) _ st = Booleana
inferType (ArrayLit xs) gt st 
  | allTypesEq (map (\ss-> inferType ss gt st) xs) = Array $ inferType (head xs) gt st
  | otherwise = CompilationError "Cannot create an array of non-homogeneous types"

-- infer type by looking up local scope first
inferType (Var id) gt st = case lookupStack st id of
  (Left s) -> case lookupTable gt id of 
                (Left s) -> CompilationError s
                (Right t) -> t
  (Right t) -> t

-- allows only integer multiplication
inferType (BinOp (Mul) e1 e2) gt st
  | t1 == Entero && t2 == Entero = Entero
  | otherwise = CompilationError $ "Cannot multiply two types other than Entero : " ++ (show t1) ++" and "++(show t2)
  where t1 = inferType e1 gt st
        t2 = inferType e2 gt st

inferType (BinOp (Add) e1 e2) gt st
  | t1 == Entero && t2 == Entero = Entero
  | otherwise = CompilationError $ "Cannot add two types other than Entero : " ++ (show t1) ++" and "++(show t2)
  where t1 = inferType e1 gt st
        t2 = inferType e2 gt st

inferType (BinOp (Sub) e1 e2) gt st
  | t1 == Entero && t2 == Entero = Entero
  | otherwise = CompilationError $ "Cannot subtract two types other than Entero : " ++ (show t1) ++" and "++(show t2)
  where t1 = inferType e1 gt st
        t2 = inferType e2 gt st

inferType (UnOp (Inv) e1) gt st
  | t1 == Entero = Entero
  | otherwise = CompilationError $ "Cannot Invert types other than Entero : " ++ (show t1)
  where t1 = inferType e1 gt st

inferType (UnOp (Not) e1) gt st
  | t1 == Booleana = Booleana
  | otherwise = CompilationError $ "Cannot negate types other than Booleana : " ++ (show t1)
  where t1 = inferType e1 gt st


inferType (BinOp (Or) e1 e2) gt st
  | t1 == Booleana && t2 == Booleana = Booleana
  | otherwise = CompilationError $ "Cannot OR two types other than Booleana : " ++ (show t1) ++" and "++(show t2)
  where t1 = inferType e1 gt st
        t2 = inferType e2 gt st

inferType (BinOp (And) e1 e2) gt st
  | t1 == Booleana && t2 == Booleana = Booleana
  | otherwise = CompilationError $ "Cannot AND two types other than Booleana : " ++ (show t1) ++" and "++(show t2)
  where t1 = inferType e1 gt st
        t2 = inferType e2 gt st

inferType (BinOp (Eq) e1 e2) gt st
  | t1 == t2 = Booleana
  | otherwise = CompilationError $ "Cannot equate two differents types : " ++ (show t1) ++" and "++(show t2)
  where t1 = inferType e1 gt st
        t2 = inferType e2 gt st

inferType (BinOp (Geq) e1 e2) gt st
  | t1 == t2 = Booleana
  | otherwise = CompilationError $ "Cannot equate two differents types : " ++ (show t1) ++" and "++(show t2)
  where t1 = inferType e1 gt st
        t2 = inferType e2 gt st
  
inferType (BinOp (Leq) e1 e2) gt st
  | t1 == t2 = Booleana
  | otherwise = CompilationError $ "Cannot equate two differents types : " ++ (show t1) ++" and "++(show t2)
  where t1 = inferType e1 gt st
        t2 = inferType e2 gt st

inferType (BinOp (Gt) e1 e2) gt st
  | t1 == t2 = Booleana
  | otherwise = CompilationError $ "Cannot equate two differents types : " ++ (show t1) ++" and "++(show t2)
  where t1 = inferType e1 gt st
        t2 = inferType e2 gt st

inferType (BinOp (Lt) e1 e2) gt st
  | t1 == t2 = Booleana
  | otherwise = CompilationError $ "Cannot equate two differents types : " ++ (show t1) ++" and "++(show t2)
  where t1 = inferType e1 gt st
        t2 = inferType e2 gt st


inferType (BinOp (Neq) e1 e2) gt st
  | t1 == t2 = Booleana
  | otherwise = CompilationError $ "Cannot equate two differents types : " ++ (show t1) ++" and "++(show t2)
  where t1 = inferType e1 gt st
        t2 = inferType e2 gt st

-- the rest of operators are quantity comparators
inferType (BinOp (_) e1 e2) gt st
  | t1 == Entero && t2 == Entero = Entero
  | otherwise = CompilationError $ "Cannot compare two types other than Entero : " ++ (show t1) ++" and "++(show t2)
  where t1 = inferType e1 gt st
        t2 = inferType e2 gt st

inferType (ArrayAccess id e) gt st | (t1 == Entero) && 
      case lookupStack st id of
        (Left _) -> False
        (Right _) -> True
      
      = case lookupStack st id of
          (Left s) -> CompilationError s
          (Right a) -> a
      | otherwise = CompilationError ("Invalid access to array "++id)

      where t1 = inferType e gt st

-- parenthesised expression
inferType (Paren e) gt st = inferType e gt st