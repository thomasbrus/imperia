module Language.Imperia.Grammar.Statement
  ( Statement (..)
  )
where

import Language.Imperia.Grammar.Expression

data Statement =
    Expression
  | Assignment String [String] Expression
  | Evaluation String [String]
  | IfElseStatement BooleanExpression Statement Statement
  | IfStatement BooleanExpression Statement
  | WhileStatement BooleanExpression Statement
  | Sequencing [Statement]
  deriving (Show)
