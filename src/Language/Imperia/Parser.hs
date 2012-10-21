module Language.Imperia.Parser (parse) where

import Prelude hiding (True, False)

import Text.Parsec hiding (parse)
import Text.Parsec.Expr
import Text.Parsec.IndentParsec (runGIPT)

import Control.Monad.Identity

import Text.Parsec.IndentParsec.Prim
import Text.Parsec.IndentParsec.Combinator


import Language.Imperia.Grammar.Expression
import Language.Imperia.Grammar.Statement
import Language.Imperia.Lexer

type Parser a = IndentParsecT String () Identity a

parse :: String -> Statement
parse input = case runIdentity $ runGIPT parser () "" input of
                Left err -> error $ show err
                Right res -> res

parser :: Parser Statement
parser = statement

empty :: Parser Statement
empty = do return $ Sequencing []

statement :: Parser Statement
statement = do
  ls <- many statement'
  return $ if length ls == 1 then head ls else Sequencing ls

statement' :: Parser Statement
statement' =
      try ifElseStatement
  <|> ifStatement
  <|> try unlessElseStatement
  <|> unlessStatement
  <|> whileStatement
  <|> untilStatement
  <|> assignStatement

ifElseStatement :: Parser Statement
ifElseStatement = do
  (IfStatement condition consequent) <- ifStatement
  reserved "else"
  alternative <- blockOf statement
  return $ IfElseStatement condition consequent alternative

ifStatement :: Parser Statement
ifStatement = do
  reserved "if"
  condition <- booleanExpression
  consequent <- blockOf statement
  return $ IfStatement condition consequent

unlessElseStatement :: Parser Statement
unlessElseStatement = do
  (IfStatement condition consequent) <- unlessStatement
  reserved "else"
  alternative <- blockOf statement
  return $ IfElseStatement condition consequent alternative

unlessStatement :: Parser Statement
unlessStatement = do
  reserved "unless"
  condition <- booleanExpression
  consequent <- blockOf statement
  return $ IfStatement (LogicalNegation condition) consequent  

whileStatement :: Parser Statement
whileStatement = do
  reserved "while"
  condition <- booleanExpression
  consequent <- blockOf statement
  return $ WhileStatement condition consequent

untilStatement :: Parser Statement
untilStatement = do
  reserved "until"
  condition <- booleanExpression
  consequent <- blockOf statement
  return $ WhileStatement (LogicalNegation condition) consequent

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
  [ [ Prefix (operator "-" >> return (ArithmeticNegation)) ]
  , [ Infix (operator "^"  >> return (ArithmeticOperation Exponentiation)) AssocRight]
  , [ Infix (operator "*"  >> return (ArithmeticOperation Multiplication)) AssocLeft
    , Infix (operator "/"  >> return (ArithmeticOperation Division)) AssocLeft
    ]
  , [ Infix (operator "+"  >> return (ArithmeticOperation Addition)) AssocLeft
    , Infix (operator "-"  >> return (ArithmeticOperation Subtraction)) AssocLeft
    ]
 ]
 
booleanOperators =
  [ [Prefix ((operator "not" <|> operator "!") >> return (LogicalNegation))]
  , [Infix ((operator "and" <|> operator "&&")  >> return (BooleanOperation And)) AssocLeft]
  , [Infix ((operator "or" <|> operator "||") >> return (BooleanOperation Or)) AssocLeft]
  ]

arithmeticTerm =
      parens arithmeticExpression
  <|> fmap Variable identifier
  <|> fmap Constant integer

booleanTerm =
      parens booleanExpression
  <|> (reserved "true"  >> return True)
  <|> (reserved "false" >> return False)
  <|> relationalExpression

relationalExpression = do
  a1 <- arithmeticExpression
  operation <- relation
  a2 <- arithmeticExpression
  return $ RelationalOperation operation a1 a2

relation =
      (reserved "<"   >> return LessThan)
  <|> (reserved "<="  >> return LessThanOrEqual)
  <|> (reserved ">"   >> return GreaterThan)
  <|> (reserved ">="  >> return GreaterThanOrEqual)
  <|> (reserved "=="  >> return Equal)
  <|> (reserved "!="  >> return NotEqual)


