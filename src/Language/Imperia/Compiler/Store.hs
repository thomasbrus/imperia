module Language.Imperia.Compiler.Store
  ( Address
  , Store (..)
  , emptyStore
  , locateByReference
  )
where

import Data.List (find)

type Address = Int
data Store = Memory
  { memoryOffset :: Int
  , registerOffset :: Int
  , references :: [(String, Address)]
  }
  deriving (Show)

emptyStore :: Store
emptyStore = Memory { memoryOffset = 1, registerOffset = 1, references = [] }

locateByReference :: Store -> String -> Address
locateByReference store label =
  case find (\mapping -> label == fst mapping) (references store) of
    Just (_, address) -> address
    Nothing -> error $ "Could not locate '" ++ label ++ "' by reference"


