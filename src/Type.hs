module Type where

import qualified Data.Text as T

data Type
  = TpInt
  | TpBool
  | TpString
  | TpFun [Type] Type
  | TpInfix Type Type Type
  | TpTuple [Type]
  | TpUnresolved T.Text Type
  | TpUnknown
  deriving (Show, Eq)
