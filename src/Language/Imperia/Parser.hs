module Language.Imperia.Parser (parse) where

import Prelude hiding (True, False)

import Text.Parsec hiding (between, parse)
import Text.Parsec.Expr
import Text.Parsec.IndentParsec (runGIPT)

import Control.Monad.Identity

import Text.Parsec.IndentParsec.Prim hiding (Block)
import Text.Parsec.IndentParsec.Combinator

import Language.Imperia.Grammar
import Language.Imperia.Lexer

import Data.Maybe

type Parser a = IndentParsecT String () Identity a

parse :: String -> Expression
parse input =
  let resultOrError = runIdentity $ runGIPT parser () "" input
  in either (\err -> error $ show err) (\res -> res) resultOrError 


parser :: Parser Expression
parser = expressions

expressions :: Parser Expression
expressions = do
  list <- choice [ many1 expression, do { eof; return [] } ]
  return $ if length list == 1 then head list else Sequencing list

expression :: Parser Expression
expression = choice
  [ try simpleExpression
  , try assignment
  , try call
  , try list
  , parens expression
  , ifElseBlock
  , whileBlock
  , untilBlock
  ]

inlineOrBlock :: Parser Expression
inlineOrBlock = (try $ blockOf expressions) <|> expression

ifElseBlock :: Parser Expression
ifElseBlock = do
  (condition, consequent) <- ifBlock <|> unlessBlock
  alternative <- option (Sequencing []) elseBlock
  return $ IfThenElse condition consequent alternative

ifBlock :: Parser (Expression, Expression)
ifBlock = do
  keyword <- reserved "if"
  condition <- expression
  optional $ reserved "then"
  consequent <- inlineOrBlock
  return $ (condition, consequent)

elseBlock :: Parser Expression
elseBlock = do
  reserved "else"
  alternative <- inlineOrBlock
  return $ alternative

unlessBlock :: Parser (Expression, Expression)
unlessBlock = do
  reserved "unless"
  condition <- expression
  optional $ reserved "then"
  consequent <- inlineOrBlock
  return $ (LogicalNegation condition, consequent)

whileBlock :: Parser Expression
whileBlock = do
  reserved "while"
  condition <- expression
  optional $ reserved "do"
  consequent <- inlineOrBlock
  return $ While condition consequent

untilBlock :: Parser Expression
untilBlock = do
  reserved "until"
  condition <- expression
  optional $ reserved "do"
  consequent <- inlineOrBlock
  return $ While (LogicalNegation condition) consequent

assignment :: Parser Expression
assignment = do
  label <- identifier
  args <- option [] $ parens (sepBy identifier comma) 
  reserved "="
  assignable <- inlineOrBlock
  return $ Assignment label args assignable

call :: Parser Expression
call = do
  callee <- identifier
  args <- option [] $ parens (sepBy expression comma)
  return $ Call callee args

list :: Parser Expression
list = do
  elements <- brackets (sepBy expression comma) 
  return $ List elements

simpleExpression :: Parser Expression
simpleExpression = buildExpressionParser (arithmeticOperators ++ booleanOperators ++ relationalOperators) term

arithmeticOperators =
  [ [ Prefix (operator "-" >> return ArithmeticNegation) ]
  , [ Infix (operator "^"  >> return Exponentiation) AssocRight]
  , [ Infix (operator "*"  >> return Multiplication) AssocLeft
    , Infix (operator "/"  >> return Division) AssocLeft
    ]
  , [ Infix (operator "+"  >> return Addition) AssocLeft
    , Infix (operator "-"  >> return Subtraction) AssocLeft
    ]
  ]

booleanOperators =
  [ [Prefix ((operator "not" <|> operator "!") >> return LogicalNegation)]
  , [Infix ((operator "and" <|> operator "&&")  >> return And) AssocLeft]
  , [Infix ((operator "or" <|> operator "||") >> return Or) AssocLeft]
  ]

relationalOperators =
  [ [ Infix (operator "<="  >> return LessThan) AssocLeft
    , Infix (operator "<"  >> return LessThanOrEqual) AssocLeft
    , Infix (operator ">="  >> return GreaterThan) AssocLeft
    , Infix (operator ">"  >> return GreaterThanOrEqual) AssocLeft
    , Infix ((operator "!=" <|> operator "isnt") >> return NotEqual) AssocLeft
    , Infix ((operator "==" <|> operator "is") >> return Equal) AssocLeft
    ]
  ]

arithmeticTerm :: Parser Expression
arithmeticTerm = choice [ fmap Constant integer ]

booleanTerm :: Parser Expression
booleanTerm = choice [ (reserved "true") >> return True, (reserved "false") >> return False ]

term :: Parser Expression
term = choice
  [ parens expression
  , ifElseBlock
  , whileBlock
  , untilBlock
  , arithmeticTerm
  , booleanTerm
  ]

