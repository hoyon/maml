module Env
  ( createEnv
  , EnvStack
  , Env
  , Binding(..)
  , findEnv
  , putEnv
  ) where

import AST
import Data.List (elemIndices, lookup)
import qualified Data.Set as S
import Error
import Protolude
import Type

data Binding
  = BdVal TyExpr
          Expr
  | BdLocal TyExpr
  | BdFun TyExpr
          [Text]
          Expr
  deriving (Show)

type BindingPair = (Text, Binding)

type Env = [BindingPair]

type EnvStack = [Env]

-- | Get the name of a declaration
name :: Dec -> Text
name (Val x _) = x
name (Fun x _ _) = x

-- | Get all names defined in AST
names :: AST -> [Text]
names = fmap name

-- | The number of times each element appears in the list
numOccurances :: (Ord a, Eq a) => [a] -> [(Int, a)]
numOccurances xs =
  let set = S.fromList xs
  in S.toList $ S.map (\x -> (length $ elemIndices x xs, x)) set

-- | Duplicate entries in list and the number of times they occur
duplicates :: [Text] -> [(Int, Text)]
duplicates xs = filter (\(x, _) -> x > 1) occurances
  where
    occurances = numOccurances xs

-- | Check for any names defined multiple times
checkNames :: AST -> ErrWarn AST
checkNames ast =
  case dupes of
    (x:_) -> throwError . makeErr $ x
    _ -> return ast
  where
    dupes = duplicates $ names ast
    makeErr (n, x) = MultipleDefinition x n

-- | Create a list of all bindings
createEnv :: AST -> ErrWarn Env
createEnv ast = map makeEnv <$> checkNames ast

makeEnv :: Dec -> (Text, Binding)
makeEnv (Val x expr) = (x, BdVal (TVarExpr $ fresh $ x <> "1") expr)
makeEnv (Fun x args expr) = (x, BdFun (funType $ length args) args expr)
  where
    funType count = makeFunType (map (TVarExpr . fresh . makeName) [0..count])
    makeName n = x <> show n

-- EnvStack operations
-- | Find the topmost binding for a name
findEnv :: Text -> EnvStack -> Maybe Binding
findEnv _ [] = Nothing
findEnv n (x:xs) =
  case lookup n x of
    Just b -> Just b
    Nothing -> findEnv n xs

-- | Put binding in first appearance of name, else add to top of stack
putEnv :: Text -> Binding -> EnvStack -> EnvStack
putEnv n binding s@(h:t) =
  case findEnv n s of
    Nothing -> ((n, binding) : h) : t
    Just _ -> snd $ mapAccumL f False s
  where
    f inserted env =
      if inserted
        then (True, env)
        else case lookup n env of
               Just _ -> (True, addToAL env n binding)
               Nothing -> (False, env)
putEnv n _ _ = panic $ "putEnv: Empty env when putting " <> n

-- Association list funcs from https://github.com/jgoerzen/missingh/blob/master/src/Data/List/Utils.hs
{- | Adds the specified (key, value) pair to the given list, removing any
existing pair with the same key already present. -}
addToAL :: Eq key => [(key, elt)] -> key -> elt -> [(key, elt)]
addToAL l key value = (key, value) : delFromAL l key

{- | Removes all (key, value) pairs from the given list where the key
matches the given one. -}
delFromAL :: Eq key => [(key, a)] -> key -> [(key, a)]
delFromAL l key = filter (\a -> fst a /= key) l
