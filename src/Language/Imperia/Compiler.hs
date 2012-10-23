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

--compile' store (Assignment label expression) =
--  (compile' store expression) ++ 
--  ( store',
--    [ Store (Imm )

--    ]
--  )
--  where
--    references' = (references store) ++ [(label, )]
--    store' = store { references = references' }

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


