module Language.Imperia.Embedded.Expression
  ( ArithmeticalExpression (..)
  , BooleanExpression (..)
  , Expression
  )
where

import Prelude hiding (True, False)
import Data.Maybe

data ArithmeticalExpression =
  Addition ArithmeticalExpression ArithmeticalExpression |
  Subtraction ArithmeticalExpression ArithmeticalExpression |
  Multiplication ArithmeticalExpression ArithmeticalExpression |
  Division ArithmeticalExpression ArithmeticalExpression |
  Exponentiation ArithmeticalExpression ArithmeticalExpression |
  Variable String |
  Constant String |
  Value Int
  deriving (Show)

data BooleanExpression =
  True |
  False |
  And BooleanExpression BooleanExpression |
  Or BooleanExpression BooleanExpression |
  LessThan ArithmeticalExpression ArithmeticalExpression |
  LessThanOrEqual ArithmeticalExpression ArithmeticalExpression |
  GreaterThan ArithmeticalExpression ArithmeticalExpression |
  GreaterThanOrEqual ArithmeticalExpression ArithmeticalExpression |
  Equal ArithmeticalExpression ArithmeticalExpression |
  NotEqual ArithmeticalExpression ArithmeticalExpression
  deriving (Show)

type Expression = Either ArithmeticalExpression BooleanExpression

example1 :: Expression
example1 = Left $ Subtraction (Value 2) (Addition (Value 1) (Variable "n"))

example2 :: Expression
example2 = Right $ And (LessThan (Addition (Variable "x") (Value 4)) (Constant "C")) True


