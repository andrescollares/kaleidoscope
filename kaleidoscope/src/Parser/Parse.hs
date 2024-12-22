{-# LANGUAGE OverloadedStrings #-}

module Parser.Parse where

import Data.Bifunctor (second)
import Data.Functor.Identity (Identity)
import Data.String (IsString (fromString))
import LLVM.AST.Name (Name)
import qualified LLVM.IRBuilder.Module as M
import qualified Parser.Lexer as L
import qualified Syntax as S (Declaration (..), TopLevel (..), Expr (..), Declaration (..), Type (..))
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

binary :: String -> Ex.Assoc -> Ex.Operator String () Identity S.Expr
binary s = Ex.Infix (L.reservedOp s >> return (S.BinOp (fromString s)))

unary :: String -> Ex.Operator String () Identity S.Expr
unary s = Ex.Prefix (L.reservedOp s >> return (S.UnaryOp (fromString s)))

unops :: Ex.OperatorTable String () Identity S.Expr
unops =
  [ [unary "!"],
    [unary "-"],
    [ unary "fst",
      unary "snd"
    ],
    [ unary "head",
      unary "tail"
    ]
  ]

binops :: Ex.OperatorTable String () Identity S.Expr
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

unop :: Ex.Operator String () Identity S.Expr
unop = Ex.Prefix (S.UnaryOp <$> op)

binop :: Ex.Operator String () Identity S.Expr
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

int :: Parser S.Expr
int =
  S.Int <$> L.int

floating :: Parser S.Expr
floating =
  S.Float <$> L.float

bool :: Parser S.Expr
bool =
  S.Bool <$> L.bool

tuple :: Parser S.Expr
tuple = do
  elements <- L.parens $ L.commaSep expr
  case elements of
    [frst, scnd] -> return $ S.TupleI frst scnd
    _ -> fail "tuples must have exactly two elements"

list :: Parser S.Expr
list = do
  elements <- L.brackets $ L.commaSep expr
  return $ S.List elements

expr :: Parser S.Expr
expr = Ex.buildExpressionParser (binops ++ unops ++ [[unop], [binop]]) factor

variable :: Parser S.Expr
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
  value <- try floating <|> try int <|> try bool <|> try tuple <|> try list
  return $ S.Constant tpi (fromString name) value

call :: Parser S.Expr
call = do
  name <- L.identifier
  -- parenthesis are optional for functions without arguments
  arguments <- L.parens $ L.commaSep expr
  return $ S.Call (fromString name) arguments

ifthen :: Parser S.Expr
ifthen = do
  L.reserved "if"
  cond <- expr
  L.reserved "then"
  thenTopLevel <- expr
  L.reserved "else"
  S.If cond thenTopLevel <$> expr

letins :: Parser S.Expr
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

factor :: Parser S.Expr
factor =
  try floating
    <|> try int
    <|> try bool
    <|> try tuple
    <|> try list
    <|> try call
    <|> try ifthen
    <|> try letins
    <|> variable
    <|> L.parens expr

parseDeclaration :: Parser S.Declaration
parseDeclaration = try function <|> try extern <|> try constant

parseTopLevel :: Parser S.TopLevel
parseTopLevel = do
  declaration <- optionMaybe parseDeclaration
  case declaration of
    Just d -> return $ S.Declaration d
    Nothing -> do
      S.Expr <$> (expr <|> factor)

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace L.lexer
  r <- p
  eof
  return r

toplevel :: Parser [S.TopLevel]
toplevel = many $ do
  def <- parseTopLevel
  L.reservedOp ";"
  return def

parseToplevel :: String -> Either ParseError [S.TopLevel]
parseToplevel = parse (contents toplevel) "<stdin>"
