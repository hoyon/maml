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
  | Application T.Text Expr
  deriving Show

data Dec
  = Val T.Text Expr
  | Fun [T.Text] T.Text Expr
  | Seq [Dec]
  deriving Show

-- | Split declarations onto their own line each
showDec :: Dec -> String
showDec (Seq x) = unlines $ map show x
showDec s       = show s
