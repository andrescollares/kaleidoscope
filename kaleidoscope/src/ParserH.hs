{-# LANGUAGE OverloadedStrings #-}

module ParserH where

import Data.Bifunctor (second)
import Data.Functor.Identity (Identity)
import Data.String (IsString (fromString))
import LLVM.AST.Name (Name)
import qualified LLVM.IRBuilder.Module as M
import Lexer
import Syntax (Declaration (..), Expr (..), Operand (..), Type (..))
import Text.Parsec
  ( ParseError,
    eof,
    many,
    optionMaybe,
    parse,
    try,
    (<|>),
  )
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

binary :: String -> Ex.Assoc -> Ex.Operator String () Identity Operand
binary s = Ex.Infix (reservedOp s >> return (BinOp (fromString s)))

unary :: String -> Ex.Operator String () Identity Operand
unary s = Ex.Prefix (reservedOp s >> return (UnaryOp (fromString s)))

unops :: Ex.OperatorTable String () Identity Operand
unops =
  [ [unary "!"],
    [unary "-"],
    [ unary "fst",
      unary "snd"
    ],
    [ unary "isInt",
      unary "head",
      unary "tail",
      unary "isDouble",
      unary "isBool",
      unary "isTuple",
      unary "isList"
    ]
  ]

binops :: Ex.OperatorTable String () Identity Operand
binops =
  [ [ binary "*" Ex.AssocLeft,
      binary "/" Ex.AssocLeft
    ],
    [ binary "+" Ex.AssocLeft,
      binary "-" Ex.AssocLeft
    ],
    [ binary "<" Ex.AssocLeft,
      binary ">" Ex.AssocLeft,
      binary "<=" Ex.AssocLeft,
      binary ">=" Ex.AssocLeft,
      binary "==" Ex.AssocLeft,
      binary "!=" Ex.AssocLeft
    ],
    [ binary "^^" Ex.AssocLeft,
      binary "&&" Ex.AssocLeft,
      binary "||" Ex.AssocLeft
    ]
  ]

op :: Parser Name
op = do
  whitespace
  o <- operator
  whitespace
  return (fromString o)

unop :: Ex.Operator String () Identity Operand
unop = Ex.Prefix (UnaryOp <$> op)

binop :: Ex.Operator String () Identity Operand
binop = Ex.Infix (BinOp <$> op) Ex.AssocLeft

tp :: Parser Syntax.Type
tp = do
  try double
    <|> try integer
    <|> try boolean
    <|> try tupleT
    <|> try listT
    <|> try funT

argument :: Parser (Type, String)
argument = do
  t <- tp
  n <- identifier
  return (t, n)

double :: Parser Syntax.Type
double = reserved "double" >> return Double

integer :: Parser Syntax.Type
integer = reserved "int" >> return Integer

boolean :: Parser Syntax.Type
boolean = reserved "bool" >> return Boolean

tupleT :: Parser Syntax.Type
tupleT = do
  reserved "tuple"
  types <- parens $ commaSep tp
  return $ Tuple (head types) (head $ tail types)

listT :: Parser Syntax.Type
listT = do
  listTp <- brackets tp
  return $ ListType listTp

funT :: Parser Syntax.Type
funT = reserved "fun" >> return FunType

int :: Parser Operand
int =
  Int <$> Lexer.int

floating :: Parser Operand
floating =
  Float <$> float

bool :: Parser Operand
bool =
  Bool <$> Lexer.bool

tuple :: Parser Operand
tuple = do
  elements <- parens $ commaSep expr
  case elements of
    [frst, scnd] -> return $ TupleI frst scnd
    _ -> fail "tuples must have exactly two elements"

list :: Parser Operand
list = do
  elements <- brackets $ commaSep expr
  return $ List elements

expr :: Parser Operand
expr = Ex.buildExpressionParser (binops ++ unops ++ [[unop], [binop]]) factor

variable :: Parser Operand
variable =
  Var . fromString <$> identifier

extern :: Parser Declaration
extern = do
  reserved "extern"
  name <- identifier
  arguments <- parens $ commaSep argument
  reserved "->"
  Extern (fromString name) (second fromString <$> arguments) <$> tp

function :: Parser Declaration
function = do
  reserved "def"
  name <- identifier
  arguments <- parens $ commaSep argument
  reserved "->"
  retType <- tp
  reserved ":"
  Function (fromString name) (second (M.ParameterName . fromString) <$> arguments) retType <$> expr

constant :: Parser Declaration
constant = do
  reservedOp "const"
  tpi <- tp
  name <- identifier
  value <- try floating <|> try ParserH.int <|> try ParserH.bool <|> try tuple <|> try list
  return $ Constant tpi (fromString name) value

typedef :: Parser Declaration
typedef = do
  reserved "type"
  name <- identifier
  reservedOp "="
  tpi <- tp
  return $ TypeDef (fromString name) tpi

call :: Parser Operand
call = do
  name <- identifier
  -- parenthesis are optional for functions without arguments
  arguments <- parens $ commaSep expr
  return $ Call (fromString name) arguments

ifthen :: Parser Operand
ifthen = do
  reserved "if"
  cond <- expr
  reserved "then"
  thenExpr <- expr
  reserved "else"
  If cond thenExpr <$> expr

letins :: Parser Operand
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

factor :: Parser Operand
factor =
  try floating
    <|> try ParserH.int
    <|> try ParserH.bool
    <|> try tuple
    <|> try list
    <|> try call
    <|> try ifthen
    <|> try letins
    <|> variable
    <|> parens expr

parseDeclaration :: Parser Declaration
parseDeclaration = try function <|> try extern <|> try constant <|> try typedef

parseExpr :: Parser Expr
parseExpr = do
  declaration <- optionMaybe parseDeclaration
  case declaration of
    Just d -> return $ TopLevel d
    Nothing -> do
      Operand <$> (expr <|> factor)

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = many $ do
  def <- parseExpr
  reservedOp ";"
  return def

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel = parse (contents toplevel) "<stdin>"
