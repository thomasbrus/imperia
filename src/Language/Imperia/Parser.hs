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

type Parser a = IndentParsecT String () Identity a

parse :: String -> Expression
parse input = case runIdentity $ runGIPT parser () "" input of
                Left err -> error $ show err
                Right res -> res

parser :: Parser Expression
parser =  expressions

expressions :: Parser Expression
expressions = do
  ls <- many $ foldedLinesOf expression -- parens expression) <|> 
  return $ if length ls == 1 then head ls else Sequencing ls

expression :: Parser Expression
expression = choice
  [ try blockExpression
  , try callExpression
  , try assignExpression
  , try $ fmap (Expression . Right) booleanExpression
  , fmap (Expression . Left) arithmeticExpression
  , try ifElseExpression
  , ifExpression
  , try unlessElseExpression
  , unlessExpression
  , whileExpression
  , untilExpression
  , nilExpression
  ]

ifElseExpression :: Parser Expression
ifElseExpression = do
  (IfExpression condition consequent) <- ifExpression
  reserved "else"
  alternative <- blockOf expressions
  return $ IfElseExpression condition consequent alternative

ifExpression :: Parser Expression
ifExpression = do
  reserved "if"
  condition <- booleanExpression
  optional $ reserved "then"
  consequent <- blockOf expressions
  return $ IfExpression condition consequent

unlessElseExpression :: Parser Expression
unlessElseExpression = do
  (IfExpression condition consequent) <- unlessExpression
  reserved "else"
  alternative <- blockOf expressions
  return $ IfElseExpression condition consequent alternative

unlessExpression :: Parser Expression
unlessExpression = do
  reserved "unless"
  condition <- booleanExpression
  optional $ reserved "then"
  consequent <- blockOf expressions
  return $ IfExpression (LogicalNegation condition) consequent  

whileExpression :: Parser Expression
whileExpression = do
  reserved "while"
  condition <- booleanExpression
  optional $ reserved "do"
  consequent <- blockOf expressions
  return $ WhileExpression condition consequent

untilExpression :: Parser Expression
untilExpression = do
  reserved "until"
  condition <- booleanExpression
  optional $ reserved "do"
  consequent <- blockOf expressions
  return $ WhileExpression (LogicalNegation condition) consequent

blockExpression :: Parser Expression
blockExpression = do
  args <- between pipe pipe (sepBy1 identifier whitespace)
  body <- expression <|> blockOf expressions
  return $ Block args body

callExpression :: Parser Expression
callExpression = do
  callee <- identifier
  operator "<-"
  args <- sepBy1 ((parens expression) <|> expression) whitespace
  return $ Call callee args

assignExpression :: Parser Expression
assignExpression = do
  variable <- identifier
  operator "->"
  assignable <- blockOf expressions
  return $ Assignment variable assignable

nilExpression :: Parser Expression
nilExpression = do
  _ <- reserved "nil"
  return $ Nil

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

arithmeticTerm :: Parser ArithmeticExpression
arithmeticTerm =
      parens arithmeticExpression
  <|> fmap Variable identifier
  <|> fmap Constant integer

booleanTerm :: Parser BooleanExpression
booleanTerm =
      parens booleanExpression
  <|> (reserved "true"  >> return True)
  <|> (reserved "false" >> return False)
  <|> relationalExpression

relationalExpression :: Parser BooleanExpression
relationalExpression = do
  a1 <- arithmeticExpression
  operation <- relation
  a2 <- arithmeticExpression
  return $ RelationalOperation operation a1 a2

relation :: Parser RelationalOperator
relation =
      (reserved "<="  >> return LessThanOrEqual)
  <|> (reserved "<"   >> return LessThan)
  <|> (reserved ">="  >> return GreaterThanOrEqual)
  <|> (reserved ">"   >> return GreaterThan)
  <|> (reserved "!="  >> return NotEqual)
  <|> (reserved "=="  >> return Equal)


