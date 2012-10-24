module Language.Imperia.Compiler (compile) where

import Processor.Sprockell as Sprockell (Assembly (..), Value (..), OpCode (..))

import Processor.Exec (exec')
import Language.Imperia.Parser

import Language.Imperia.Grammar as Grammar
import Language.Imperia.Compiler.Store
import qualified Language.Imperia.Compiler.Operation as Operation
-- import qualified Language.Imperia.Compiler.ArithmeticCompiler as ArithmeticCompiler
-- import qualified Language.Imperia.Compiler.BooleanCompiler as BooleanCompiler

import Data.List

compile :: Expression -> (Store, [Assembly])
compile expression = (store, assembly ++ [ EndProg ])
  where (store, assembly) = compile' emptyStore expression
  
compile' :: Store -> Expression -> (Store, [Assembly])
compile' store (Sequencing expressions) =
  foldl (\(store, assembly) expression ->
    let (store', assembly') = compile' store expression
    in (store', assembly ++ assembly')
  ) (store, []) expressions

compile' store (Constant int) =
  ( store,
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
  Operation.perform compile' store Sprockell.And expr1 expr2

compile' store (Grammar.Or expr1 expr2) = 
  Operation.perform compile' store Sprockell.Or expr1 expr2

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

compile' store (IfElseExpression test expr1 expr2) =
  ( store
  , -- Evaluate the condition
    condition ++
    [ -- Set the condition flag
      Calc Eq (offset + 1) 0 0
      -- Jump to alternative if false
    , RCJump $ length consequent + 3
    ] ++

    -- Otherwise evaluate the consequent
    consequent ++
    [ -- And copy the outcome
      Calc Add (offset + 2) 0 offset
      -- Finally jump to the end and skip the alternative
    , RJump $ length alternative + 2
    ] ++

    -- Evaluate the alternative
    alternative ++
    [ -- Copy it's outcome
      Calc Add (offset + 3) 0 offset
    ]
  )
  where
    offset = registerOffset store
    condition = snd $ compile' (store { registerOffset = offset + 1 }) test
    consequent = snd $ compile' (store { registerOffset = offset + 2 }) expr1
    alternative = snd $ compile' (store { registerOffset = offset + 3 }) expr2

compile' store (WhileExpression test expression) =
  ( store
  , -- Evaluate the condition
    condition ++
    [ -- Set the condition flag
      Calc Eq (offset + 1) 0 0
      -- Skip to end if false
    , RCJump $ length consequent + 3
    ] ++

    -- Otherwise evaluate the consequent
    consequent ++
    [ -- Copy the outcome (this might be the last iteration)
      Calc Add (offset + 2) 0 offset
      -- Return to the while condition
    , RJump $ - (length consequent) - (length condition) - 3
    ]
  )
  where
    offset = registerOffset store
    condition = snd $ compile' (store { registerOffset = offset + 1 }) test
    consequent = snd $ compile' (store { registerOffset = offset + 2 }) expression


--fetchFromMemory :: Store -> Address -> Int
--fetchFromMemory store address = (memory store) !! address

--locateInMemory :: Store -> Int -> Maybe Address
--locateInMemory store int = findIndex ((==) int) (memory store)

--locateByReference :: Store -> String -> Address
--locateByReference store label =
--  case find (\mapping -> label == fst mapping) (references store) of
--    Just (_, address) -> address
--    Nothing -> error $ "Could not locate '" ++ label ++ "' by reference"


