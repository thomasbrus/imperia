module Language.Imperia.Compiler.Store
  ( Address
  , Store (..)
  , emptyStore
  , shiftRegisterOffset
  , shiftMemoryOffset
  , findAddress
  , chooseAddress
  , createReference
  , updateReference
  , destroyReference
  , createUniqueReference
  )
where

import Data.List
import Data.Maybe

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
findAddress store name =
  case lookupAddress store name of
    Just address -> address
    Nothing -> error $ "Could not find reference: '" ++ name ++ "'"

chooseAddress :: Store -> String -> Address
chooseAddress store name =
  case lookupAddress store name of
    Just address -> address
    Nothing -> memoryOffset store

createReference :: Store -> Reference -> Store
createReference store reference = store { references = [reference] ++ (references store) }

destroyReference :: Store -> Reference -> Store
destroyReference store reference =
  let isMatch (name, _) (name', _) = name == name'
  in store { references = deleteBy isMatch reference (references store) }

updateReference :: Store -> Reference -> Store
updateReference store reference = createReference (destroyReference store reference) reference

createUniqueReference :: Store -> Reference -> Store
createUniqueReference store reference@(name, address)
  | isJust $ lookupAddress store name
  = error $ "Could not create reference. '" ++ name ++ "' has already been assigned."
  | otherwise
  = createReference store reference

lookupAddress :: Store -> String -> Maybe Address
lookupAddress store name = lookup name (references store)


