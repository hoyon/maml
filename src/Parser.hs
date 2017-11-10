{-# LANGUAGE OverloadedStrings #-}
module Parser where

import qualified Data.Text                  as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Expr

data Constant
  = Number Integer
  | Str T.Text
  | Bool Bool
  deriving Show

data Expr
  = Con Constant
  | While Expr Expr
  | Tuple [Expr]
  | Id T.Text
  | Infix T.Text Expr Expr
  deriving Show

data Dec
  = Val T.Text Expr
  | Fun [T.Text] T.Text Expr
  | Seq [Dec]
  deriving Show

type Parser = Parsec Void T.Text

sc :: Parser ()
sc = L.space space1 line block
  where
    line = L.skipLineComment ";;"
    block = L.skipBlockCommentNested "(*" "*)"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

integer :: Parser Integer
integer = L.signed sc (lexeme L.decimal)

semi :: Parser T.Text
semi = symbol ";"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

reserved :: T.Text -> Parser ()
reserved w = lexeme (string w *> notFollowedBy alphaNumChar)

identifier :: Parser T.Text
identifier = (lexeme . try) $ T.pack <$> p
  where
    p = (:) <$> letterChar <*> many alphaNumChar

expr :: Parser Expr
expr = makeExprParser term ops <?> "expression"

term :: Parser Expr
term = parens expr
   <|> whileExpr
   <|> tupleExpr
   <|> idExpr
   <|> Con . Number <$> integer
   <?> "term"

ops :: [[Operator Parser Expr]]
ops = [ [ InfixL (Infix <$> symbol "*")
        , InfixL (Infix <$> symbol "/")]
      , [ InfixL (Infix <$> symbol "+")
        , InfixL (Infix <$> symbol "-")]
      ]

whileExpr :: Parser Expr
whileExpr = do
  reserved "while"
  exp1 <- expr
  reserved "do"
  exp2 <- expr
  return $ While exp1 exp2

tupleExpr :: Parser Expr
tupleExpr = parens $ Tuple <$> sepBy expr (symbol ",")

idExpr :: Parser Expr
idExpr = Id <$> identifier

dec :: Parser Dec
dec = f <$> sepEndBy dec' semi
  where
    f [x] = x
    f x   = Seq x

dec' :: Parser Dec
dec' = valDec
  <|> funDec
  <?> "declaration"

valDec :: Parser Dec
valDec = do
  reserved "val"
  name <- identifier
  _ <- symbol "="
  exp1 <- expr
  return $ Val name exp1

funDec :: Parser Dec
funDec = do
  reserved "fun"
  name <- identifier
  args <- arg
  _ <- symbol "="
  exp1 <- expr
  return $ Fun args name exp1
  where
    arg :: Parser [T.Text]
    arg = (pure <$> identifier)
      <|> parens (sepBy identifier (symbol ","))

lang :: Parser Dec
lang = between sc eof dec

showDec :: Dec -> String
showDec (Seq x) = unlines $ map show x
showDec s = show s

parseString :: T.Text -> IO ()
parseString s = case parse lang "file" s of
  Right x -> putStrLn $ showDec x
  Left e -> putStrLn $ parseErrorPretty e
