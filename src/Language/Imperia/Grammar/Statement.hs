module Language.Imperia.Grammar.Statement
  ( Program
  , Statement (..)
  )
where

import Language.Imperia.Grammar.Expression

type Program = [Statement]

data Statement =
  Assignment String ArithmeticExpression |
  IfElseStatement BooleanExpression Statement Statement |
  IfStatement BooleanExpression Statement |
  WhileStatement BooleanExpression Statement |
  Sequencing [Statement]
  deriving (Show)


