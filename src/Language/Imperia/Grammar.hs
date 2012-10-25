module Language.Imperia.Grammar (Expression (..)) where

import Prelude hiding (True, False)

data Expression =
    Assignment String [String] Expression
  | Call String [Expression]
  | List [Expression]
  | ListAssignment [[String]] [Expression]
  | Splat String
  | IfThenElse Expression Expression Expression
  | While Expression Expression
  | Sequencing [Expression]
  | Variable String
  | Constant Integer
  | Addition Expression Expression
  | Subtraction Expression Expression
  | Multiplication Expression Expression
  | Division Expression Expression
  | Exponentiation Expression Expression
  | ArithmeticNegation Expression
  | And Expression Expression
  | Or Expression Expression
  | LessThan Expression Expression
  | LessThanOrEqual Expression Expression
  | GreaterThan Expression Expression
  | GreaterThanOrEqual Expression Expression
  | Equal Expression Expression
  | NotEqual Expression Expression
  | LogicalNegation Expression
  | True
  | False
  deriving (Show)

