module Language.Imperia.Compiler.Store
  ( Address
  , Store (..)
  , emptyStore
  , shiftRegisterOffset
  , shiftMemoryOffset
  , findAddress
  , createReference
  )
where

import Data.List (find)
import Data.Maybe (isJust)

type Address = Int
type Reference = (String, Address)

data Store = Memory
  { registerOffset :: Int
  , memoryOffset :: Int
  , references :: [Reference]
  }
  deriving (Show)

emptyStore :: Store
emptyStore = Memory { registerOffset = 1, memoryOffset = 1, references = [] }

shiftRegisterOffset :: Store -> Int -> Store
shiftRegisterOffset store amount = store { registerOffset = (registerOffset store) + amount }

shiftMemoryOffset :: Store -> Int -> Store
shiftMemoryOffset store amount = store { memoryOffset = (memoryOffset store) + amount }

findAddress :: Store -> String -> Address
findAddress store label =
  case lookupAddress store label of
    Just reference -> reference
    Nothing -> error $ "Could not locate reference: '" ++ label ++ "'"

createReference :: Store -> Reference -> Store
createReference store reference = store { references = (references store) ++ [reference] }

createUniqueReference :: Store -> Reference -> Store
createUniqueReference store reference@(label, address)
  | isJust $ lookupAddress store label
  = error $ "Could not create reference. '" ++ label ++ "' has already been assigned."
  | otherwise
  = createReference store reference

lookupAddress :: Store -> String -> Maybe Address
lookupAddress store label = lookup label (references store)


