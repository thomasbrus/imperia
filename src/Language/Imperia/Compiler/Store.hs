module Language.Imperia.Compiler.Store
  ( Address
  , Store (..)
  , emptyStore
  )
where

type Address = Int
data Store = Memory
  { memory :: [Int]
  , registerOffset :: Int
  , references :: [(String, Address)]
  }

emptyStore = Memory { memory = [], registerOffset = 2, references = [] }
-- testStore = emptyStore { references = [("test", 1)] }
