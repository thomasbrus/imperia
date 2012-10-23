module Language.Imperia.Compiler.BooleanCompiler (compile) where

import Processor.Sprockell (Assembly (..), Value (..), OpCode (..))

import Language.Imperia.Grammar hiding (BooleanOperator (..))
import qualified Language.Imperia.Grammar as Grammar (BooleanOperator (..), BooleanExpression(..))
import Language.Imperia.Compiler.Store
import qualified Language.Imperia.Compiler.Operation as Operation

import qualified Language.Imperia.Compiler.ArithmeticCompiler as ArithmeticCompiler

compile :: Store -> BooleanExpression -> (Store, [Assembly])
compile store booleanExpression = compile' store booleanExpression

compile' :: Store -> BooleanExpression -> (Store, [Assembly])
compile' store booleanExpression =
  case booleanExpression of
    Grammar.True ->
      (store, [ Store (Imm 1) (registerOffset store) ])

    Grammar.False ->
      (store, [ Store (Imm 0) (registerOffset store) ])

    BooleanOperation Grammar.And expr1 expr2 ->
      perform store And expr1 expr2

    BooleanOperation Grammar.Or expr1 expr2 ->
      perform store Or expr1 expr2

    RelationalOperation LessThan expr1 expr2 ->
      perform' store Lt expr1 expr2

    RelationalOperation LessThanOrEqual expr1 expr2 ->
      compile' store $ BooleanOperation Grammar.Or
        (RelationalOperation LessThan expr1 expr2)
        (RelationalOperation Equal expr1 expr2)

    RelationalOperation GreaterThan expr1 expr2 ->
      perform' store Gt expr1 expr2

    RelationalOperation GreaterThanOrEqual expr1 expr2 ->
      compile' store $ BooleanOperation Grammar.Or
        (RelationalOperation GreaterThan expr1 expr2)
        (RelationalOperation Equal expr1 expr2)

    RelationalOperation Equal expr1 expr2 ->
      perform' store Eq expr1 expr2

    RelationalOperation NotEqual expr1 expr2 ->
      perform' store NEq expr1 expr2

    LogicalNegation expression ->
      perform store Not expression Grammar.False

perform :: Store -> OpCode -> BooleanExpression -> BooleanExpression -> (Store, [Assembly])
perform store opCode expr1 expr2 = Operation.perform compile store opCode expr1 expr2

perform' :: Store -> OpCode -> ArithmeticExpression -> ArithmeticExpression -> (Store, [Assembly])
perform' store opCode expr1 expr2 = Operation.perform ArithmeticCompiler.compile store opCode expr1 expr2


