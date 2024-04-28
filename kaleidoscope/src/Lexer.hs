module Lexer where

import Control.Applicative (many, (<|>))
import Syntax (Type (..))
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = ["+", "*", "-", "/", ":", ";", ",", "<", ">", "|", "->"]
    names = ["const", "def", "extern", "if", "then", "else", "binary", "unary", "in", "for", "fst", "snd"]
    style =
      emptyDef
        { Tok.commentLine = "#",
          Tok.reservedOpNames = ops,
          Tok.reservedNames = names
        }

int :: Parser Integer
int = Tok.integer lexer

float :: Parser Double
float = Tok.float lexer

bool :: Parser Bool
bool = Tok.lexeme lexer $ (True <$ Tok.symbol lexer "true") <|> (False <$ Tok.symbol lexer "false")

-- tuple :: Parser (Type, Type)
-- tuple = Tok.parens lexer $ do
--   t1 <- 
--   _ <- Tok.comma lexer
--   t2 <- type'
--   return (t1, t2)

identifier :: Parser String
identifier = Tok.identifier lexer

type' :: Parser Type
type' = do
  t <- identifier
  case t of
    "double" -> return Syntax.Double
    "int" -> return Syntax.Integer
    "bool" -> return Syntax.Boolean
    "tuple" -> do
      reserved "<"
      t1 <- type'
      reserved ","
      t2 <- type'
      reserved ">"
      return $ Syntax.Tuple t1 t2
    _ -> fail "unknown type"

argument :: Parser (Type, String)
argument = do
  t <- type'
  n <- identifier
  return (t, n)

parens :: Parser a -> Parser a
parens = Tok.parens lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

operator :: Parser String
operator = do
  c <- Tok.opStart emptyDef
  cs <- many $ Tok.opLetter emptyDef
  return (c : cs)

whitespace :: Parser ()
whitespace = Tok.whiteSpace lexer
