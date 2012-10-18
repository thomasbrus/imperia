module Language.Imperia.Grammar.Statement where

import Language.Imperia.Grammar.Expression

type Program = [Statement]

data Statement =
  Assignment String ArithmeticExpression |
  IfThenElse BooleanExpression [Statement] [Statement] |
  WhileLoop BooleanExpression [Statement] |
  Sequencing [Statement]
  deriving (Show)


