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
import Data.ByteString.Short
import qualified Data.Bifunctor

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
  _ <- int
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

int :: Parser Expr
int =
  Float . fromInteger <$> integer

floating :: Parser Expr
floating =
  Float <$> float

expr :: Parser Expr
expr = Ex.buildExpressionParser (binops ++ [[unop], [binop]]) factor

variable :: Parser Expr
variable =
  Var . fromString <$> identifier

extern :: Parser Expr
extern = do
  reserved "extern"
  name <- identifier
  arguments <- parens $ many identifier
  return $ Extern (fromString name) (map fromString arguments)

function :: Parser Expr
function = do
  reserved "def"
  name <- identifier
  arguments <- parens $ many identifier
  Function (fromString name) (map (M.ParameterName . fromString) arguments) <$> expr

constant :: Parser Expr
constant = do
  reservedOp "const"
  name <- identifier
  value <- try float <|> try (fromInteger <$> integer)
  return $ Constant (fromString name) value


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
  reserved "var"
  defs <- commaSep $ do
    var <- identifier
    reservedOp "="
    val <- expr
    return (var, val)
  reserved "in"
  body <- expr
  return $ foldr (uncurry Let . Data.Bifunctor.first fromString) body defs

factor :: Parser Expr
factor =
  try floating
    <|> try int
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
