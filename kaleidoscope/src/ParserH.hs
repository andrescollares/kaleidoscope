{-# LANGUAGE OverloadedStrings #-}

module ParserH where

import Data.Bifunctor (second)
import Data.Functor.Identity (Identity)
import Data.String (IsString (fromString))
import LLVM.AST.Name (Name)
import qualified LLVM.IRBuilder.Module as M
import qualified Lexer as L
import qualified Syntax  as S (Declaration (..), Expr (..), Operand (..), Type (..))
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

binary :: String -> Ex.Assoc -> Ex.Operator String () Identity S.Operand
binary s = Ex.Infix (L.reservedOp s >> return (S.BinOp (fromString s)))

unary :: String -> Ex.Operator String () Identity S.Operand
unary s = Ex.Prefix (L.reservedOp s >> return (S.UnaryOp (fromString s)))

unops :: Ex.OperatorTable String () Identity S.Operand
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

binops :: Ex.OperatorTable String () Identity S.Operand
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
  L.whitespace
  o <- L.operator
  L.whitespace
  return (fromString o)

unop :: Ex.Operator String () Identity S.Operand
unop = Ex.Prefix (S.UnaryOp <$> op)

binop :: Ex.Operator String () Identity S.Operand
binop = Ex.Infix (S.BinOp <$> op) Ex.AssocLeft

tp :: Parser S.Type
tp = do
  try double
    <|> try integer
    <|> try boolean
    <|> try tupleT
    <|> try listT
    <|> try funT

argument :: Parser (S.Type, String)
argument = do
  t <- tp
  n <- L.identifier
  return (t, n)

double :: Parser S.Type
double = L.reserved "double" >> return S.Double

integer :: Parser S.Type
integer = L.reserved "int" >> return S.Integer

boolean :: Parser S.Type
boolean = L.reserved "bool" >> return S.Boolean

tupleT :: Parser S.Type
tupleT = do
  L.reserved "tuple"
  types <- L.parens $ L.commaSep tp
  return $ S.Tuple (head types) (head $ tail types)

listT :: Parser S.Type
listT = do
  listTp <- L.brackets tp
  return $ S.ListType listTp

funT :: Parser S.Type
funT = do
  L.reserved "fun"
  args <- L.parens $ L.commaSep tp
  L.reserved "->"
  S.FunType args <$> tp


int :: Parser S.Operand
int =
  S.Int <$> L.int

floating :: Parser S.Operand
floating =
  S.Float <$> L.float

bool :: Parser S.Operand
bool =
  S.Bool <$> L.bool

tuple :: Parser S.Operand
tuple = do
  elements <- L.parens $ L.commaSep expr
  case elements of
    [frst, scnd] -> return $ S.TupleI frst scnd
    _ -> fail "tuples must have exactly two elements"

list :: Parser S.Operand
list = do
  elements <- L.brackets $ L.commaSep expr
  return $ S.List elements

expr :: Parser S.Operand
expr = Ex.buildExpressionParser (binops ++ unops ++ [[unop], [binop]]) factor

variable :: Parser S.Operand
variable =
  S.Var . fromString <$> L.identifier

extern :: Parser S.Declaration
extern = do
  L.reserved "extern"
  name <- L.identifier
  arguments <- L.parens $ L.commaSep argument
  L.reserved "->"
  S.Extern (fromString name) (second fromString <$> arguments) <$> tp

function :: Parser S.Declaration
function = do
  L.reserved "def"
  name <- L.identifier
  arguments <- L.parens $ L.commaSep argument
  L.reserved "->"
  retType <- tp
  L.reserved ":"
  S.Function (fromString name) (second (M.ParameterName . fromString) <$> arguments) retType <$> expr

constant :: Parser S.Declaration
constant = do
  L.reservedOp "const"
  tpi <- tp
  name <- L.identifier
  value <- try floating <|> try ParserH.int <|> try ParserH.bool <|> try tuple <|> try list
  return $ S.Constant tpi (fromString name) value

typedef :: Parser S.Declaration
typedef = do
  L.reserved "type"
  name <- L.identifier
  L.reservedOp "="
  S.TypeDef (fromString name) <$> tp

call :: Parser S.Operand
call = do
  name <- L.identifier
  -- parenthesis are optional for functions without arguments
  arguments <- L.parens $ L.commaSep expr
  return $ S.Call (fromString name) arguments

ifthen :: Parser S.Operand
ifthen = do
  L.reserved "if"
  cond <- expr
  L.reserved "then"
  thenExpr <- expr
  L.reserved "else"
  S.If cond thenExpr <$> expr

letins :: Parser S.Operand
letins = do
  L.reserved "let"
  defs <- L.commaSep $ do
    tpi <- tp
    var <- L.identifier
    L.reservedOp "="
    val <- expr
    return (tpi, fromString var, val)
  L.reserved "in"
  body <- expr
  return $ foldr (\(t, n, v) -> S.Let t n v) body defs

factor :: Parser S.Operand
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
    <|> L.parens expr

parseDeclaration :: Parser S.Declaration
parseDeclaration = try function <|> try extern <|> try constant <|> try typedef

parseExpr :: Parser S.Expr
parseExpr = do
  declaration <- optionMaybe parseDeclaration
  case declaration of
    Just d -> return $ S.TopLevel d
    Nothing -> do
      S.Operand <$> (expr <|> factor)

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace L.lexer
  r <- p
  eof
  return r

toplevel :: Parser [S.Expr]
toplevel = many $ do
  def <- parseExpr
  L.reservedOp ";"
  return def

parseToplevel :: String -> Either ParseError [S.Expr]
parseToplevel = parse (contents toplevel) "<stdin>"
