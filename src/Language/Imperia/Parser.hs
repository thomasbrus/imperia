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
parse input = case runIdentity $ runGIPT parser () "" input of
                Left err -> error $ show err
                Right res -> res

parser :: Parser Expression
parser =  expressions

expressions :: Parser Expression
expressions = do
  ls <- many $ foldedLinesOf expression
  return $ if length ls == 1 then head ls else Sequencing ls

expression :: Parser Expression
expression = choice
  [ try blockExpression
  , try callExpression
  , try assignExpression
  , ifElseExpression
  , unlessElseExpression
  , whileExpression
  , untilExpression
  , simpleExpression
  , nilExpression
  ]

ifElseExpression :: Parser Expression
ifElseExpression = do
  (condition, consequent) <- ifExpression
  res <- optionMaybe elseExpression
  alternative <- if isJust res then (do return $ fromJust res) else (do return $ Sequencing [])
  return $ IfElseExpression condition consequent alternative

ifExpression :: Parser (Expression, Expression)
ifExpression = do
  reserved "if"
  condition <- simpleExpression
  optional $ reserved "then"
  consequent <- expression <|> blockOf expression
  return $ (condition, consequent)

elseExpression :: Parser Expression
elseExpression = do
  reserved "else"
  alternative <- expression <|> blockOf expression
  return $ alternative

unlessElseExpression :: Parser Expression
unlessElseExpression = do
  (condition, consequent) <- unlessExpression
  res <- optionMaybe elseExpression
  alternative <- if isJust res then (do return $ fromJust res) else (do return $ Sequencing [])
  return $ IfElseExpression condition consequent alternative

unlessExpression :: Parser (Expression, Expression)
unlessExpression = do
  reserved "unless"
  condition <- simpleExpression
  optional $ reserved "then"
  consequent <- expression <|> blockOf expression
  return $ (LogicalNegation condition, consequent)

whileExpression :: Parser Expression
whileExpression = do
  reserved "while"
  condition <- simpleExpression
  optional $ reserved "do"
  consequent <- (try $ blockOf expressions) <|> expression
  return $ WhileExpression condition consequent

untilExpression :: Parser Expression
untilExpression = do
  reserved "until"
  condition <- simpleExpression
  optional $ reserved "do"
  consequent <- (try $ blockOf expressions) <|> expression
  return $ WhileExpression (LogicalNegation condition) consequent  

blockExpression :: Parser Expression
blockExpression = do
  args <- between pipe pipe (sepBy1 identifier whitespace)
  body <- (try $ blockOf expressions) <|> expression
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
  assignable <- (try $ blockOf expressions) <|> expression
  return $ Assignment variable assignable

nilExpression :: Parser Expression
nilExpression = do
  _ <- reserved "nil"
  return $ Nil

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
    , Infix (operator "!="  >> return NotEqual) AssocLeft
    , Infix (operator "=="  >> return Equal) AssocLeft
    ]
  ]

term :: Parser Expression
term = choice [ parens expression, arithmeticTerm, booleanTerm ]

arithmeticTerm :: Parser Expression
arithmeticTerm = choice [ fmap Variable identifier, fmap Constant integer ]

booleanTerm :: Parser Expression
booleanTerm = choice [ (reserved "true") >> return True, (reserved "false") >> return False ]


