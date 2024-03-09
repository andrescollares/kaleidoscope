{-# LANGUAGE OverloadedStrings #-}

module ParserH where

import Data.Functor.Identity
import Lexer
import Syntax
import Text.Parsec
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import qualified LLVM.IRBuilder.Module as M
import Data.String
import Data.Bifunctor (second)
import Data.ByteString.Short (ShortByteString)

binary :: String -> Ex.Assoc -> Ex.Operator String () Identity Expr
binary s = Ex.Infix (reservedOp s >> return (BinOp (fromString s)))

binops :: Ex.OperatorTable String () Identity Expr
binops =
  [ [ binary "*" Ex.AssocLeft,
      binary "/" Ex.AssocLeft
    ],
    [ binary "+" Ex.AssocLeft,
      binary "-" Ex.AssocLeft
    ],
    [ binary "<" Ex.AssocLeft,
      binary ">" Ex.AssocLeft
    ]
  ]

binarydef :: Parser Expr
binarydef = do
  reserved "def"
  reserved "binary"
  o <- op
  _ <- ParserH.int
  args <- parens $ many identifier
  BinaryDef o (map fromString args) <$> expr

unarydef :: Parser Expr
unarydef = do
  reserved "def"
  reserved "unary"
  o <- op
  args <- parens $ many identifier
  UnaryDef o (map fromString args) <$> expr

op :: Parser ShortByteString
op = do
  whitespace
  o <- operator
  whitespace
  return (fromString o)

unop :: Ex.Operator String () Identity Expr
unop = Ex.Prefix (UnaryOp <$> op)

binop :: Ex.Operator String () Identity Expr
binop = Ex.Infix (BinOp <$> op) Ex.AssocLeft

tp :: Parser Syntax.Type
tp = do
  try double
    <|> try integer
    <|> try boolean

double :: Parser Syntax.Type
double = reserved "double" >> return Double

integer :: Parser Syntax.Type
integer = reserved "int" >> return Integer

boolean :: Parser Syntax.Type
boolean = reserved "bool" >> return Boolean

int :: Parser Expr
int =
  Int <$> Lexer.int

floating :: Parser Expr
floating =
  Float <$> float

bool :: Parser Expr
bool =
  Bool <$> Lexer.bool

expr :: Parser Expr
expr = Ex.buildExpressionParser (binops ++ [[unop], [binop]]) factor

variable :: Parser Expr
variable =
  Var . fromString <$> identifier

extern :: Parser Expr
extern = do
  reserved "extern"
  name <- identifier
  arguments <- parens $ commaSep argument
  reserved "->"
  Extern (fromString name) (second fromString <$> arguments) <$> tp

function :: Parser Expr
function = do
  reserved "def"
  name <- identifier
  arguments <- parens $ commaSep argument
  reserved "->"
  retType <- tp
  reserved ":"
  Function (fromString name) (second (M.ParameterName . fromString) <$> arguments) retType <$> expr

constant :: Parser Expr
constant = do
  reservedOp "const"
  tpi <- tp
  name <- identifier
  value <- try floating <|> try ParserH.int <|> try ParserH.bool
  return $ Constant tpi (fromString name) value


call :: Parser Expr
call = do
  name <- identifier
  -- parenthesis are optional for functions without arguments
  arguments <- parens $ commaSep expr
  return $ Call (fromString name) arguments

ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- expr
  reserved "then"
  thenExpr <- expr
  reserved "else"
  If cond thenExpr <$> expr

letins :: Parser Expr
letins = do
  reserved "let"
  defs <- commaSep $ do
    tpi <- tp
    var <- identifier
    reservedOp "="
    val <- expr
    return (tpi, fromString var, val)
  reserved "in"
  body <- expr
  return $ foldr (\(t, n, v) -> Let t n v) body defs

factor :: Parser Expr
factor =
  try floating
    <|> try ParserH.int
    <|> try ParserH.bool
    <|> try extern
    <|> try function
    <|> try call
    <|> try ifthen
    <|> try letins
    <|> variable
    <|> parens expr

defn :: Parser Expr
defn =
  try extern
    <|> try function
    <|> try constant
    <|> try binarydef
    <|> try unarydef
    <|> expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = many $ do
  def <- defn
  reservedOp ";"
  return def

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel = parse (contents toplevel) "<stdin>"
