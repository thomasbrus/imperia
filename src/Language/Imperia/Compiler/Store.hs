module Language.Imperia.Compiler.Store
  ( Address
  , Store (..)
  , emptyStore
  )
where

type Address = Int
data Store = Memory
  { memoryOffset :: Int
  , registerOffset :: Int
  , references :: [(String, Address)]
  }
  deriving (Show)

emptyStore :: Store
emptyStore = Memory { memoryOffset = 1, registerOffset = 1, references = [] }

