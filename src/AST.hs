module AST where

import Protolude

data Constant
  = Number Integer
  | Str Text
  | Bool Bool
  deriving Show

data Expr
  = Con Constant
  | While Expr Expr
  | If Expr Expr Expr
  | Tuple [Expr]
  | Id Text
  | Infix Text Expr Expr
  | Appl Expr Expr
  deriving Show

data Dec
  = Val Text Expr
  | Fun Text [Text] Expr
  deriving Show

type AST = [Dec]

-- | Split declarations onto their own line each
showAST :: AST -> Text
showAST = foldMap ((`mappend` "\n") . show)
