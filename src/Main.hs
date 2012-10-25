module Main
  ( main
  , executeFile
  )
where

import System.Environment
import Processor.Sprockell as Sprockell (Assembly (..), Value (..), OpCode (..))

import Processor.Exec (exec')
import Language.Imperia.Parser
import Language.Imperia.Grammar
import Language.Imperia.Compiler
import Language.Imperia.Compiler.Store

main :: IO ()
main = do
  [filename] <- getArgs
  result <- executeFile filename
  putStrLn result

executeFile :: String -> IO (String)
executeFile filename = do
  contents <- readFile filename
  ast <- do return $ parse contents
  (_, assembly) <- do return $ compile ast
  (_, register, _, _) <- do return $ last $ exec' $ assembly
  return $ show $ register !! 1

