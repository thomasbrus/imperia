module Language.Imperia.Definition (definition) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

definition = emptyDef 
  { Token.commentStart    = "/*"
  , Token.commentEnd      = "*/"
  , Token.commentLine     = "//"
  , Token.identStart      = letter
  , Token.identLetter     = alphaNum
  , Token.reservedNames   =
    [ "if"
    , "then"
    , "else"
    , "while"
    , "do"
    , "true"
    , "false"
    , "not"
    , "and"
    , "or"
    ]
  , Token.reservedOpNames =
    [ "+", "-", "*", "^", "/", "="
    , "<", "<=", ">", ">=", "==", "!="
    , "and", "or", "not"
    ]
  }

