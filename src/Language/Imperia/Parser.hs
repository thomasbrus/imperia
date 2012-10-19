module Language.Imperia.Parser (parse) where

import Prelude hiding (True, False)

import Control.Monad
import Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as Parsec (parse)
import Text.ParserCombinators.Parsec.Expr

import Language.Imperia.Grammar.Expression
import Language.Imperia.Grammar.Statement
import Language.Imperia.Lexer

parse :: String -> Statement
parse input = case Parsec.parse parser "" input of
  Left err  -> error $ show err
  Right res -> res

parser :: Parser Statement
parser = whitespace >> statement

statement :: Parser Statement
statement = parens statement <|> sequenceOfStatements
 
sequenceOfStatements = do
  list <- (endBy statement' semicolon)
  return (Sequencing list)

statement' :: Parser Statement
statement' = ifStatement <|> whileStatement <|> assignStatement

example = unlines
  [ "a = 3;"
  , "n = 5;"
  , "power = 1;"
  , "while (n != 0) do ("
  , "  power = a * power;"
  , "  n = n - 1;"
  , ");"
  ]

ifStatement :: Parser Statement
ifStatement = do
  reserved "if"
  condition <- booleanExpression
  reserved "then"
  consequent <- statement
  reserved "else"
  alternative <- statement
  return $ IfThenElse condition consequent alternative

whileStatement :: Parser Statement
whileStatement = do
  reserved "while"
  condition <- booleanExpression
  reserved "do"
  consequent <- statement
  return $ WhileLoop condition consequent
 
assignStatement :: Parser Statement
assignStatement = do
  variable <- identifier
  operator "="
  expression <- arithmeticExpression
  return $ Assignment variable expression

arithmeticExpression :: Parser ArithmeticExpression
arithmeticExpression = buildExpressionParser arithmeticOperators arithmeticTerm

booleanExpression :: Parser BooleanExpression
booleanExpression = buildExpressionParser booleanOperators booleanTerm

arithmeticOperators =
  [ [Infix (reserved "+"  >> return (ArithmeticOperation Addition)) AssocLeft]
  , [Infix (reserved "-"  >> return (ArithmeticOperation Subtraction)) AssocLeft]
  , [Infix (reserved "*"  >> return (ArithmeticOperation Multiplication)) AssocLeft]
  , [Infix (reserved "/"  >> return (ArithmeticOperation Division)) AssocLeft]
  , [Infix (reserved "^"  >> return (ArithmeticOperation Exponentiation)) AssocLeft]
  , [Prefix (reserved "-" >> return (ArithmeticNegation)) ]
  ]
 
booleanOperators =
  [ [Infix (reserved "and"  >> return (BooleanOperation And)) AssocLeft]
  , [Infix (reserved "or"   >> return (BooleanOperation Or)) AssocLeft]
  , [Prefix (reserved "not" >> return (LogicalNegation)) ]
  ]

arithmeticTerm =
      parens arithmeticExpression
  <|> liftM Variable identifier
  <|> liftM Value integer

booleanTerm =
      parens booleanExpression
  <|> (reserved "true"  >> return (True))
  <|> (reserved "false" >> return (False))
  <|> relationalExpression

relationalExpression = do
  a1 <- arithmeticExpression
  operation <- relation
  a2 <- arithmeticExpression
  return $ RelationalOperation operation a1 a2

relation =
      (reserved "<" >> return LessThan)
  <|> (reserved "<="  >> return LessThanOrEqual)
  <|> (reserved ">"   >> return GreaterThan)
  <|> (reserved ">="  >> return GreaterThanOrEqual)
  <|> (reserved "=="  >> return Equal)
  <|> (reserved "!="  >> return NotEqual)


