module Language.Imperia.Embedded.Statement where

import Language.Embedded.Expression

type Program = [Statement]
type Sequencing = [Statement]

data Statement =
  Assignment String Expression |
  IfThenElse BooleanExpression Sequencing Sequencing |
  WhileLoop BooleanExpression Sequencing |
  -- ForLoop (String, Expression) BooleanExpression (String, Expression) Sequencing |
  Sequencing Sequencing
  deriving (Show)

example1 :: Statement
example1 = Sequencing
  [ Assignment "a" (Left $ Addition (Value 1) (Value 2))
  , Assignment "b" (Right $ LessThan (Variable "a") (Value 10))
  ]

example2 :: Statement
example2 = Sequencing
  [ Assignment "i" (Left $ (Value 0))
  , WhileLoop (LessThan (Variable "i") (Value 4))
    [ Assignment "i" (Left $ (Addition (Variable "i") (Value 1)))
    ]
  ]

