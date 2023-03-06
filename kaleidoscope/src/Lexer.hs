module Lexer where

import Text.Parsec.Language (emptyDef)
import Text.Parsec.Prim (many)

import qualified Text.Parsec.Token as Tok
import Data.ByteString.Short (pack)

-- lexer :: Tok.TokenParser Byte
lexer = Tok.makeTokenParser style
  where
    ops = ["+","*","-","/",";",",","<"]
    names = ["def","extern","if","then","else","in","for"]
    style = emptyDef {
               Tok.commentLine = "#"
             , Tok.reservedOpNames = map pack ops
             , Tok.reservedNames = names
             }

integer    = Tok.integer lexer
float      = Tok.float lexer
parens     = Tok.parens lexer
commaSep   = Tok.commaSep lexer
semiSep    = Tok.semiSep lexer
identifier = Tok.identifier lexer
whitespace = Tok.whiteSpace lexer
reserved   = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
