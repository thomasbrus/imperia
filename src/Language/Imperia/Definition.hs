{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.Imperia.Definition (definition) where

import Text.Parsec
import Text.Parsec.Token (GenLanguageDef(..))

definition = LanguageDef
  { commentStart    = ""
  , commentEnd      = ""
  , commentLine     = "#"
  , identStart      = letter
  , identLetter     = alphaNum
  , opStart         = oneOf "-+/*^=<>"
  , opLetter        = oneOf "-+/*^=<>"
  , caseSensitive   = True
  , nestedComments  = True
  , reservedNames   =
    [ "if" , "unless"
    , "then", "else"
    , "while", "until", "do"
    , "true", "false"
    , "not", "and", "or"
    , "nil", "->"
    ]
  , reservedOpNames =
    [ "+", "-", "*", "^", "/"
    , "<", "<=", ">", ">=", "==", "!="
    , "&&", "||", "!"
    ]
  }
