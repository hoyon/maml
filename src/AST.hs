module AST where

import qualified Data.Text as T

data Constant
  = Number Integer
  | Str T.Text
  | Bool Bool
  deriving Show

data Expr
  = Con Constant
  | While Expr Expr
  | If Expr Expr Expr
  | Tuple [Expr]
  | Id T.Text
  | Infix T.Text Expr Expr
  | Appl Expr Expr
  deriving Show

data Dec
  = Val T.Text Expr
  | Fun T.Text [T.Text] Expr
  deriving Show

type AST = [Dec]

-- | Split declarations onto their own line each
showAST :: AST -> String
showAST xs = unlines $ map show xs
