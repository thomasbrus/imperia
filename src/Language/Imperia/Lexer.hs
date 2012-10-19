module Language.Imperia.Lexer
  ( lexer
  , identifier
  , reserved
  , operator
  , parens
  , integer
  , semicolon
  , whitespace
  )
where

import qualified Text.ParserCombinators.Parsec.Token as Token
import Language.Imperia.Definition

lexer = Token.makeTokenParser definition

identifier  = Token.identifier lexer
reserved    = Token.reserved lexer
operator    = Token.reservedOp lexer
parens      = Token.parens lexer
integer     = Token.integer lexer
semicolon   = Token.semi lexer
whitespace  = Token.whiteSpace lexer