module Type where

import Protolude

data Type
  = TpInt
  | TpBool
  | TpString
  | TpFun [Type] Type
  | TpInfix Type Type Type
  | TpTuple [Type]
  | TpUnresolved Text Type
  | TpUnknown
  deriving (Show, Eq)
