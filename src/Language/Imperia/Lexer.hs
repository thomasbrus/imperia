module Language.Imperia.Lexer
  ( lexer
  , identifier
  , reserved
  , operator
  , parens
  , brackets
  , integer
  , whitespace
  , comma
  )
where

import Control.Monad.Identity
import Text.Parsec.Token (makeTokenParser)
import qualified Text.Parsec.IndentParsec.Token as IndentToken
import Language.Imperia.Definition (definition)

lexer :: IndentToken.IndentTokenParser String () Identity
lexer = makeTokenParser definition

identifier  = IndentToken.identifier lexer
reserved    = IndentToken.reserved lexer
operator    = IndentToken.reservedOp lexer
parens      = IndentToken.parens lexer
brackets    = IndentToken.brackets lexer
integer     = IndentToken.integer lexer
whitespace  = IndentToken.whiteSpace lexer
comma       = IndentToken.comma lexer