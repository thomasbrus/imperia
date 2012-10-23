module Language.Imperia.Compiler.Arithmetic (compile) where

import Processor.Sprockell (Assembly (..), Value (..), OpCode (..))

import Language.Imperia.Grammar
import Language.Imperia.Compiler.Store

compile :: Store -> ArithmeticExpression -> (Store, [Assembly])
compile store arithmeticExpression =
  case arithmeticExpression of
    Constant int ->
      (store, [ Store (Imm (fromIntegral int)) 1 ])

    ArithmeticNegation expression ->
      compile store $ ArithmeticOperation Subtraction (Constant 0) expression

    ArithmeticOperation Addition expr1 expr2 ->
      perform store Add expr1 expr2

    ArithmeticOperation Subtraction expr1 expr2 ->
      perform store Sub expr1 expr2

    ArithmeticOperation Multiplication expr1 expr2 ->
      perform store Mul expr1 expr2

    ArithmeticOperation Division expr1 expr2 ->
      perform store Div expr1 expr2

    ArithmeticOperation Exponentiation expr1 expr2 ->
      error "Exponentiation is not yet implemented"

perform :: Store -> OpCode -> ArithmeticExpression -> ArithmeticExpression -> (Store, [Assembly])
perform store opCode expr1 expr2 =
  let
    offset = registerOffset store
    store' = store { registerOffset = offset + 2 }
  in
    ( store,
      -- Load the outcome of the first expression into register
      (snd $ compile store' expr1) ++ [ Load (Addr 1) offset ] ++ 
      -- Likewise for the second expression
      (snd $ compile store' expr2) ++ [ Load (Addr 1) (offset + 1) ] ++
      -- Perform calculation and store the result into the register
      [ Calc opCode offset (offset + 1) 1
      -- Store the result from the register to memory
      , Store (Addr 1) 1
      ]
    )


