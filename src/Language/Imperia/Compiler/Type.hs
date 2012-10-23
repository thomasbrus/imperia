module Language.Imperia.Compiler.Type
  ( Type (..)
  , set
  )
where

import Processor.Sprockell (Assembly (..), Value (..))

data Type = Bool | Int

set :: Type -> [Assembly]
set Int = [ Store (Imm 1) 2 ]
set Bool = [ Store (Imm 2) 2 ]