module Parser (parseString) where

import           AST
import qualified Data.Text                  as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Expr

type Parser = Parsec Void T.Text

-- | Space consumer parser
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

reservedWords :: [T.Text]
reservedWords = [ "do"
                , "while"
                , "if"
                , "then"
                , "else"
                ]

reservedSymbols :: [T.Text]
reservedSymbols = ["+", "-", "/", "*", "<", "=", "!", ">"]

-- | Left recursive expression parser
expr :: Parser Expr
expr = makeExprParser term ops <?> "expression"

term :: Parser Expr
term = parens expr
   <|> whileExpr
   <|> ifExpr
   <|> tupleExpr
   <|> idExpr
   <|> Con . Number <$> integer
   <?> "term"

-- | Table of expression operations
ops :: [[Operator Parser Expr]]
ops = [
        [ InfixL (spacef >> pure Appl)]

      , [ binaryOp "*"
        , binaryOp "/"
        ]

      , [ binaryOp "+"
        , binaryOp "-"
        ]

      , [ binaryOp "=="
        , binaryOp "!="
        , binaryOp "<"
        , binaryOp "<="
        , binaryOp ">"
        , binaryOp ">="
        ]
      ]

-- | Convenience function for binary operations
binaryOp :: T.Text -> Operator Parser Expr
binaryOp s = InfixL (Infix <$> symbol s)

-- | space "operator" between expressions for function application
spacef :: Parser ()
spacef = sc *> notFollowedBy (choice . map reserved $ reservedSymbols ++ reservedWords)

whileExpr :: Parser Expr
whileExpr = do
  reserved "while"
  exp1 <- expr
  reserved "do"
  exp2 <- expr
  return $ While exp1 exp2

ifExpr :: Parser Expr
ifExpr = do
  reserved "if"
  p <- expr
  reserved "then"
  exp1 <- expr
  reserved "else"
  exp2 <- expr
  return $ If p exp1 exp2

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

-- | Parse text and either return the AST or an error
parseString :: T.Text -> Either T.Text Dec
parseString src = case parse lang "src" src of
  Right a -> Right a
  Left e  -> Left $ T.pack $ parseErrorPretty e
