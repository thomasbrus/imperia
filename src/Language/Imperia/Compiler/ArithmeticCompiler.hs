module Language.Imperia.Compiler.ArithmeticCompiler (compile) where

import Processor.Sprockell (Assembly (..), Value (..), OpCode (..))

import Language.Imperia.Grammar
import Language.Imperia.Compiler.Store
import qualified Language.Imperia.Compiler.Operation as Operation

compile :: Store -> Expression -> (Store, [Assembly])
compile store expression = compile' store expression

compile' :: Store -> Expression -> (Store, [Assembly])
compile' store expression =
  case expression of
    Constant int ->
      ( store,
        [ Store (Imm (fromIntegral int)) (registerOffset store) ]
      )

    Negation expression ->
      compile' store $ ArithmeticOperation Subtraction (Constant 0) expression

    Addition expr1 expr2 ->
      perform store Add expr1 expr2

    Subtraction expr1 expr2 ->
      perform store Sub expr1 expr2

    Multiplication expr1 expr2 ->
      perform store Mul expr1 expr2

    Division expr1 expr2 ->
      perform store Div expr1 expr2

    Exponentiation expr1 expr2 ->
      error "Exponentiation is not yet implemented"

perform :: Store -> OpCode -> ArithmeticExpression -> ArithmeticExpression -> (Store, [Assembly])
perform store opCode expr1 expr2 = Operation.perform compile' store opCode expr1 expr2


