module Language.Imperia.Compiler.Operation (perform) where

import Processor.Sprockell (Assembly (..), Value (..), OpCode (..))

import Language.Imperia.Compiler.Store

perform :: (Store -> a -> (Store, [Assembly])) -> Store -> OpCode -> a -> a -> (Store, [Assembly])
perform compile store opCode expr1 expr2 =
  let
    offset = registerOffset store
    store' = store { registerOffset = offset + 2 }
  in
    ( -- Leave the store as is
      store,
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


