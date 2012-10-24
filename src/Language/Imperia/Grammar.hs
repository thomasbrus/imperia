module Language.Imperia.Grammar (Expression (..)) where

import Prelude hiding (True, False)

data Expression =
    Nil
  | Assignment String Expression
  | Block [String] Expression
  | Call String [Expression]
  | IfElseExpression Expression Expression Expression
  | IfExpression Expression Expression
  | ElseExpression Expression
  | WhileExpression Expression Expression
  | Sequencing [Expression]
  | Variable String
  | Constant Integer
  | Addition Expression Expression
  | Subtraction Expression Expression
  | Multiplication Expression Expression
  | Division Expression Expression
  | Exponentiation Expression Expression
  | ArithmeticNegation Expression
  | True
  | False
  | And Expression Expression
  | Or Expression Expression
  | LessThan Expression Expression
  | LessThanOrEqual Expression Expression
  | GreaterThan Expression Expression
  | GreaterThanOrEqual Expression Expression
  | Equal Expression Expression
  | NotEqual Expression Expression
  | LogicalNegation Expression
  deriving (Show)

