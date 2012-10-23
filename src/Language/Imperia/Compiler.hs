module Language.Imperia.Compiler (compile) where

import Processor.Sprockell
import Processor.Exec

import Language.Imperia.Grammar

import Language.Imperia.Parser

import Data.List

type Address = Int
data Store = Memory
  { memory :: [Int]
  , registerOffset :: Int
  , references :: [(String, Address)]
  }

emptyStore = Memory { memory = [], registerOffset = 1, references = [] }
testStore = emptyStore { references = [("test", 1)] }

compile :: Expression -> [Assembly]
compile expression = (compile' emptyStore expression) ++ [EndProg]

compile' :: Store -> Expression -> [Assembly]
compile' store (Sequencing expression) = concat $ map (compile' store) expression
compile' store (Expression (Left arithmeticExpression)) =
  case arithmeticExpression of
    Constant int ->
      [ Store (Imm (fromIntegral int)) 1 ]
    --Variable label ->
    --  let
    --    address = locateByReference store label
    --    value   = fetchFromMemory store address
    --  in
    --    [ Store (Imm value) 1
    --    ]
    ArithmeticNegation expr ->
      compile' store $ Expression $ Left $ ArithmeticOperation Subtraction (Constant 0) expr

    ArithmeticOperation Addition expr1 expr2 ->
      evaluateArithmeticOperation store Add expr1 expr2

    ArithmeticOperation Subtraction expr1 expr2 ->
      evaluateArithmeticOperation store Sub expr1 expr2

    ArithmeticOperation Multiplication expr1 expr2 ->
      evaluateArithmeticOperation store Mul expr1 expr2

    ArithmeticOperation Division expr1 expr2 ->
      evaluateArithmeticOperation store Div expr1 expr2

    ArithmeticOperation Exponentiation expr1 expr2 ->
      error "Not yet implemented"

evaluateArithmeticOperation :: Store -> OpCode -> ArithmeticExpression -> ArithmeticExpression -> [Assembly]
evaluateArithmeticOperation store opCode expr1 expr2 =
  (compile' store' expr1') ++
  [ Load (Addr 1) (offset + 1)] ++ -- Load the outcome of the first expression into register
  (compile' store' expr2') ++
  [ Load (Addr 1) (offset + 2) ] ++ -- Likewise for the second expression
  [ Calc opCode (offset + 1) (offset + 2) 1 -- Perform calculation and store the result into the register
  , Store (Addr 1) 1 -- Store the result from the register to memory
  ]
  where
    expr1' = Expression $ Left expr1
    expr2' = Expression $ Left expr2
    offset = registerOffset store
    store' = store { registerOffset = offset + 2 }

--storeInMemory :: Store -> Int -> Address
--storeInMemory store int
--  | Just result
--  = 

fetchFromMemory :: Store -> Address -> Int
fetchFromMemory store address = (memory store) !! address

--locateOrStoreInMemory :: Store -> Int -> (Address, [Assembly])
--locateOrStoreInMemory store int
--  case locateInMemory store int of
--    Just address -> (address, [])
--    Nothing -> (..., [Store ...])

locateInMemory :: Store -> Int -> Maybe Address
locateInMemory store int = findIndex ((==) int) (memory store)

locateByReference :: Store -> String -> Address
locateByReference store label =
  case find (\mapping -> label == fst mapping) (references store) of
    Just (_, address) -> address
    Nothing -> error $ "Could not locate '" ++ label ++ "' by reference"


