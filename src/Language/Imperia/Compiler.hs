module Language.Imperia.Compiler (compile) where

import Processor.Sprockell (Assembly (..), Value (..), OpCode (..))

import Processor.Exec (exec')
import Language.Imperia.Parser

import Language.Imperia.Grammar
import Language.Imperia.Compiler.Store
import qualified Language.Imperia.Compiler.ArithmeticCompiler as ArithmeticCompiler
import qualified Language.Imperia.Compiler.BooleanCompiler as BooleanCompiler

import Data.List

compile :: Expression -> (Store, [Assembly])
compile expression = (store, assembly ++ [ EndProg ])
  where (store, assembly) = compile' emptyStore expression
  
compile' :: Store -> Expression -> (Store, [Assembly])
compile' store (Sequencing expressions) =
  foldl (\(store, assembly) expression ->
    let (store', assembly') = compile' store expression
    in (store', assembly ++ assembly'))
  (store, [])
  expressions

compile' store (Expression (Left arithmeticExpression)) =
  ArithmeticCompiler.compile store arithmeticExpression

compile' store (Expression (Right booleanExpression)) =
  BooleanCompiler.compile store booleanExpression

compile' store (IfExpression booleanExpression expression) =
  compile' store (IfElseExpression booleanExpression expression (Sequencing []))

compile' store (IfElseExpression booleanExpression expr1 expr2) =
  ( store
  , -- Evaluate the condition
    condition ++

    [ -- Now load it into the register
      Load (Addr $ offset + 2) offset
      -- And set the condition flag
    , Calc Eq offset 0 0
      -- Jump to alternative if false
    , RCJump $ length consequent + 2
    ] ++

    -- Otherwise evaluate the consequent
    consequent ++
    
    [ -- Jump to the very end and store the outcome
      RJump $ length alternative + 2
    ] ++

    -- Evaluate the alternative and store the outcome
    -- If the alternative is empty, then skip it alltogether
    (if null alternative then [] else alternative ++ [ RJump 2 ]) ++

    [ -- Skip storing the outcome
      RJump 3
    ] ++

    [ -- Store the outcome of the evaluated sub routine
      Load (Addr $ offset + 2) offset
    , Store (Addr offset) offset
    ]
  )
  where
    offset = registerOffset store
    store' = store { registerOffset = offset + 2 }
    condition = snd $ compile' store' $ Expression $ Right booleanExpression
    consequent = snd $ compile' store' expr1
    alternative = snd $ compile' store' expr2


fetchFromMemory :: Store -> Address -> Int
fetchFromMemory store address = (memory store) !! address

locateInMemory :: Store -> Int -> Maybe Address
locateInMemory store int = findIndex ((==) int) (memory store)

locateByReference :: Store -> String -> Address
locateByReference store label =
  case find (\mapping -> label == fst mapping) (references store) of
    Just (_, address) -> address
    Nothing -> error $ "Could not locate '" ++ label ++ "' by reference"


