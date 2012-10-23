module Language.Imperia.Compiler.ArithmeticCompiler (compile) where

import Processor.Sprockell (Assembly (..), Value (..), OpCode (..))

import Language.Imperia.Grammar
import Language.Imperia.Compiler.Store
import qualified Language.Imperia.Compiler.Operation as Operation

compile :: Store -> ArithmeticExpression -> (Store, [Assembly])
compile store arithmeticExpression = compile' store arithmeticExpression

compile' :: Store -> ArithmeticExpression -> (Store, [Assembly])
compile' store arithmeticExpression =
  case arithmeticExpression of
    Constant int ->
      ( store,
        [ Store (Imm (fromIntegral int)) (registerOffset store) ]
      )

    ArithmeticNegation expression ->
      compile' store $ ArithmeticOperation Subtraction (Constant 0) expression

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
perform store opCode expr1 expr2 = Operation.perform compile' store opCode expr1 expr2


