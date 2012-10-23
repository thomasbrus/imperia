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
  deriving (Show)

emptyStore :: Store
emptyStore = Memory { memory = [], registerOffset = 1, references = [] }

