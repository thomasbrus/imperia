module Language.Imperia.Compiler.Operation (perform) where

import Processor.Sprockell (Assembly (..), Value (..), OpCode (..))

import Language.Imperia.Compiler.Store

perform :: (Store -> a -> (Store, [Assembly])) -> Store -> OpCode -> a -> a -> (Store, [Assembly])
perform compile store opCode expr1 expr2 =
  let
    offset  = registerOffset store
    store'  = store { registerOffset = offset + 1 }
    store'' = store { registerOffset = offset + 2 }
  in
    ( -- Leave the store as is
      store,
      -- Evaluate the first expression
      (snd $ compile store' expr1) ++
      -- Likewise for the second expression
      (snd $ compile store'' expr2) ++
      -- Perform calculation and store the result into the register
      [ Calc opCode (offset + 1) (offset + 2) offset
      ]
    )


