module Language.Imperia.Compiler (compile) where

import Processor.Sprockell as Sprockell (Assembly (..), Value (..), OpCode (..))

import Processor.Exec (exec')
import Language.Imperia.Parser

import Language.Imperia.Grammar as Grammar
import Language.Imperia.Compiler.Store
import qualified Language.Imperia.Compiler.Operation as Operation

example = unlines
  [ "power |a, n| ->            "
  , "  if n is 0 then           "
  , "    1                      "
  , "  else                     "
  , "    a * power |a, (n - 1)| "
  , "power |2, 3|               "
  ]

compile :: Expression -> (Store, [Assembly])
compile expression = (store, assembly ++ [ EndProg ])
  where (store, assembly) = compile' emptyStore expression
  
compile' :: Store -> Expression -> (Store, [Assembly])
compile' store (Sequencing expressions) =
  foldl (\(store, assembly) expression ->
    let (store', assembly') = compile' store expression
    in (store', assembly ++ assembly')
  ) (store, []) expressions

-- TODO Replace with function definition

compile' store (Assignment label args expression) =
  ( store'
  , -- Evaluate the expression
    value ++ 
    -- Store the result in memory
    [ Store (Addr (registerOffset store)) address ]
  )
  where
    (_, value) = compile' store expression
    address = memoryOffset store
    reference = (label, address)
    -- Update the store with the new reference and memory offset
    store' = shiftMemoryOffset (createReference store reference) 1

-- TODO Replace with function call

compile' store (Variable label) =
  ( store,
    -- Copy the value into the register
    [ Load (Addr address) (registerOffset store) ]
  )
  where
    -- Find the address in memory by it's label
    address = findAddress store label

compile' store (Constant int) =
  ( store,
    -- Load the value directly into the register 
    [ Load (Imm (fromIntegral int)) (registerOffset store) ]
  )

compile' store (ArithmeticNegation expression) = 
  compile' store $ Subtraction (Constant 0) expression

compile' store (Addition expr1 expr2) = 
  Operation.perform compile' store Add expr1 expr2

compile' store (Subtraction expr1 expr2) = 
  Operation.perform compile' store Sub expr1 expr2

compile' store (Multiplication expr1 expr2) = 
  Operation.perform compile' store Mul expr1 expr2

compile' store (Division expr1 expr2) = 
  Operation.perform compile' store Div expr1 expr2

compile' store (Exponentiation expr1 expr2) = 
  error "Exponentiation is not yet implemented"

compile' store Grammar.True = 
  (store, [ Load (Imm 1) (registerOffset store) ])

compile' store Grammar.False = 
  (store, [ Load (Imm 0) (registerOffset store) ])

compile' store (Grammar.And expr1 expr2) =
  -- Any nonzero expression should evaluate to true
  Operation.perform compile' store Sprockell.And
    (NotEqual expr1 Grammar.False)
    (NotEqual expr2 Grammar.False)

compile' store (Grammar.Or expr1 expr2) =
  Operation.perform compile' store Sprockell.Or
    (NotEqual expr1 Grammar.False)
    (NotEqual expr2 Grammar.False)

compile' store (LessThan expr1 expr2) = 
  Operation.perform compile' store Lt expr1 expr2  

compile' store (LessThanOrEqual expr1 expr2) = 
  compile' store $ Grammar.Or
    (LessThan expr1 expr2)
    (Equal expr1 expr2)

compile' store (GreaterThan expr1 expr2) = 
  Operation.perform compile' store Gt expr1 expr2  

compile' store (GreaterThanOrEqual expr1 expr2)  = 
  compile' store $ Grammar.Or
    (GreaterThan expr1 expr2)
    (Equal expr1 expr2)

compile' store (Equal expr1 expr2) =
  Operation.perform compile' store Eq expr1 expr2  

compile' store (NotEqual expr1 expr2) =
  Operation.perform compile' store NEq expr1 expr2  

compile' store (LogicalNegation expression) =
  Operation.perform compile' store Not expression Grammar.False  

compile' store (IfThenElse test expr1 expr2) =
  ( store
  , -- Evaluate the condition
    condition ++
    -- Set the condition flag
    [ Calc Eq (offset + 1) 0 0
    -- Jump to alternative if false
    , RCJump $ length consequent + 3
    ] ++

    -- Otherwise evaluate the consequent
    consequent ++
    -- And copy the outcome
    [ Calc Add (offset + 2) 0 offset
    -- Finally jump to the end and skip the alternative
    , RJump $ length alternative + 2
    ] ++

    -- Evaluate the alternative
    alternative ++
    -- Copy it's outcome
    [ Calc Add (offset + 3) 0 offset ]
  )
  where
    offset = registerOffset store
    (_, condition) = compile' (shiftRegisterOffset store 1) test
    (_, consequent) = compile' (shiftRegisterOffset store 2) expr1
    (_, alternative) = compile' (shiftRegisterOffset store 3) expr2

compile' store (While test expression) =
  ( store
  , -- Evaluate the condition
    condition ++
    -- Set the condition flag
    [ Calc Eq (offset + 1) 0 0
    -- Skip to end if false
    , RCJump $ length consequent + 3
    ] ++

    -- Otherwise evaluate the consequent
    consequent ++
    -- Save the outcome each iteration (this might be the last one)
    [ Calc Add (offset + 2) 0 offset
    -- Return to the while condition
    , RJump $ - (length consequent) - (length condition) - 3
    ]
  )
  where
    offset = registerOffset store
    (_, condition) = compile' (shiftRegisterOffset store 1) test
    (_, consequent) = compile' (shiftRegisterOffset store 2) expression

