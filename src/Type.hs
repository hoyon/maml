module Type where

import           Protolude hiding (Type)

data Type
  = TpInt
  | TpBool
  | TpFun [Type] Type
  | TpInfix Type Type Type
  | TpTuple [Type]
  | TpUnresolved Text Type
  | TpUnknown
  deriving (Show, Eq)
