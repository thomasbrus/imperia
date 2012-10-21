module Language.Imperia.Grammar.Expression
  ( BooleanExpression (..)
  , BooleanOperator (..)
  , RelationalOperator (..)
  , ArithmeticExpression (..)
  , ArithmeticOperator (..)
  , Expression
  )
where

import Prelude hiding (True, False)

data BooleanExpression =
    BooleanOperation BooleanOperator BooleanExpression BooleanExpression
  | RelationalOperation RelationalOperator ArithmeticExpression ArithmeticExpression
  | LogicalNegation BooleanExpression
  | True
  | False
  deriving (Show)

data BooleanOperator =
    And
  | Or
  deriving (Show)

data RelationalOperator =
    LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  | Equal
  | NotEqual
  deriving (Show)

data ArithmeticExpression =
    ArithmeticOperation ArithmeticOperator ArithmeticExpression ArithmeticExpression
  | ArithmeticNegation ArithmeticExpression
  | Variable String
  | Constant Integer
  deriving (Show)

data ArithmeticOperator =
    Addition
  | Subtraction
  | Multiplication
  | Division
  | Exponentiation
  deriving (Show)

type Expression = Either ArithmeticExpression BooleanExpression



