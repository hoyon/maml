module Parser (parseString, lang) where

import           AST
import           Error
import           Protolude hiding (try, Infix)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Expr
import           Prelude (fail, String)

type Parser = Parsec Void Text

-- | Space consumer parser
sc :: Parser ()
sc = L.space space1 line block
  where
    line = L.skipLineComment ";;"
    block = L.skipBlockCommentNested "(*" "*)"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

integer :: Parser Int
integer = lexeme $
      L.decimal
  <|> ((\x -> 0 - x) <$> (char '~' >> L.decimal))

semi :: Parser Text
semi = symbol ";"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

reserved :: Text -> Parser ()
reserved w = lexeme (string w *> notFollowedBy alphaNumChar)

boolean :: Parser Constant
boolean = try (reserved "true" >> pure (Bool True))
  <|> try (reserved "false" >> pure (Bool False))

-- | Character which can start an identifier
idStart :: Parser Char
idStart = letterChar <|> char '_'

-- | Characters which make up the rest of the identifier
idRest :: Parser Char
idRest = alphaNumChar <|> char '_'

identifier :: Parser Text
identifier = (lexeme . try) $ toS <$> (p >>= check)
  where
    p = (:) <$> idStart <*> many idRest
    check :: String -> Parser Text
    check x =
      if toS x `elem` reservedWords
      then fail $ "Cannot use reserved keyword " ++ x ++ " as identifier"
      else return $ toS x

reservedWords :: [Text]
reservedWords = [ "do"
                , "while"
                , "if"
                , "then"
                , "else"
                , "true"
                , "false"
                ]

reservedSymbols :: [Text]
reservedSymbols = ["+", "-", "/", "*", "<", "=", "!", ">", "|", "&", "%"]

-- | Left recursive expression parser
expr :: Parser Expr
expr = makeExprParser term ops <?> "expression"

term :: Parser Expr
term = -- try tupleExpr <|>
       -- try unitExpr <|>
       parens expr
   <|> whileExpr
   <|> ifExpr
   <|> try conExpr
   <|> try callExpr
   <|> idExpr
   <?> "term"

-- | Table of expression operations
ops :: [[Operator Parser Expr]]
ops = [
        [ binaryOp "*"
        , binaryOp "/"
        , binaryOp "%"
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
        , binaryOp "||"
        , binaryOp "&&"
        ]
      ]

-- | Convenience function for binary operations
binaryOp :: Text -> Operator Parser Expr
binaryOp s = InfixL (Infix <$> symbol s)

-- | space "operator" between expressions for function application
spacef :: Parser ()
spacef = sc *> notFollowedBy (choice . map reserved $ reservedSymbols ++ reservedWords)

conExpr :: Parser Expr
conExpr = Con . Number <$> integer
  -- <|> Con <$> boolean

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
tupleExpr = parens $ do
  exp1 <- expr
  _ <- symbol ","
  rest <- sepBy1 expr (symbol ",")
  return $ Tuple (exp1 : rest)

unitExpr :: Parser Expr
unitExpr = symbol "()" >> pure (Tuple [])

idExpr :: Parser Expr
idExpr = Id <$> identifier

callExpr :: Parser Expr
callExpr = do
  fname <- identifier
  args <- try multi <|> single
  return $ Call fname args
  where
    multi = do
      Tuple args <- tupleExpr
      return args
    single = do
      args <- expr
      return [args]

dec :: Parser [Dec]
dec = sepEndBy dec' semi

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
  return $ Fun name args exp1
  where
    arg :: Parser [Text]
    arg = (pure <$> identifier)
      <|> parens (sepBy identifier (symbol ","))

lang :: Parser AST
lang = between sc eof dec

-- | Parse text and either return the AST or an error
parseString :: Text -> ErrWarn AST
parseString src = case parse lang "src" src of
  Right a -> return a
  Left e  -> throwError $ ParseError $ toS $ parseErrorPretty e
