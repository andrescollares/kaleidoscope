module ParserH where

import Data.Functor.Identity
import Lexer
import Syntax
import Text.Parsec
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

binary :: String -> Ex.Assoc -> Ex.Operator String () Identity Expr
binary s assoc = Ex.Infix (reservedOp s >> return (BinOp s)) assoc

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
  prec <- int
  args <- parens $ many identifier
  body <- expr
  return $ BinaryDef o args body

unarydef :: Parser Expr
unarydef = do
  reserved "def"
  reserved "unary"
  o <- op
  args <- parens $ many identifier
  body <- expr
  return $ UnaryDef o args body

op :: Parser String
op = do
  whitespace
  o <- operator
  whitespace
  return o

unop = Ex.Prefix (UnaryOp <$> op)

binop = Ex.Infix (BinOp <$> op) Ex.AssocLeft

int :: Parser Expr
int = do
  n <- integer
  return $ Float (fromInteger n)

floating :: Parser Expr
floating = do
  n <- float
  return $ Float n

expr :: Parser Expr
expr = Ex.buildExpressionParser (binops ++ [[unop], [binop]]) factor

variable :: Parser Expr
variable = do
  var <- identifier
  return $ Var var

function :: Parser Expr
function = do
  reserved "def"
  name <- identifier
  arguments <- parens $ many identifier
  body <- expr
  return $ Function name arguments body

extern :: Parser Expr
extern = do
  reserved "extern"
  name <- identifier
  arguments <- parens $ many identifier
  return $ Extern name arguments

call :: Parser Expr
call = do
  name <- identifier
  arguments <- parens $ commaSep expr
  return $ Call name arguments

ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- expr
  reserved "then"
  thenExpr <- expr
  reserved "else"
  elseExpr <- expr
  return $ If cond thenExpr elseExpr

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
  return $ foldr (uncurry Let) body defs

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
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = parse (contents toplevel) "<stdin>" s
