module Env ( createEnv
           , EnvStack
           , Env
           , Binding(..)
           , findEnv
           , putEnv
           ) where

import           AST
import qualified Data.Map  as Map
import qualified Data.Set  as S
import           Error
import           Protolude
import           Type
import           Data.List (elemIndices)

data Binding
  = BdVal Type Expr
  | BdFun [(Text, Type)] Type Expr
  deriving Show

type Env = Map.Map Text Binding
type EnvStack = [Env]

-- | Get the name of a declaration
name :: Dec -> Text
name (Val x _)   = x
name (Fun x _ _) = x

-- | Get all names defined in AST
names :: AST -> [Text]
names = fmap name

-- | The number of times each element appears in the list
numOccurances :: (Ord a, Eq a) => [a] -> [(Int, a)]
numOccurances xs = let set = S.fromList xs
                   in S.toList $ S.map (\x -> (length $ elemIndices x xs, x)) set

-- | Duplicate entries in list and the number of times they occur
duplicates :: [Text] -> [(Int, Text)]
duplicates xs = filter (\(x, _) -> x > 1) occurances
  where
    occurances = numOccurances xs

-- | Check for any names defined multiple times
checkNames :: AST -> ErrWarn AST
checkNames ast = case dupes of
                   (x:_) -> throwError . makeErr $ x
                   _     -> return ast
  where
    dupes = duplicates $ names ast
    makeErr (n, x) = MultipleDefinition x n

-- | Create a list of all bindings
createEnv :: AST -> ErrWarn Env
createEnv ast = Map.fromList . map makeEnv <$> checkNames ast

makeEnv :: Dec -> (Text, Binding)
makeEnv (Val x expr) = (x, BdVal TpUnknown expr)
makeEnv (Fun x args expr) = (x, BdFun [(n, TpUnknown) | n <- args] TpUnknown expr)

-- EnvStack operations

-- | Find the topmost binding for a name
findEnv :: Text -> EnvStack -> Maybe Binding
findEnv _ [] = Nothing
findEnv n (x:xs) = case Map.lookup n x of
                      Just b -> Just b
                      Nothing -> findEnv n xs

-- | Put binding in first appearance of name, else add to top of stack
putEnv :: Text -> Binding -> EnvStack -> EnvStack
putEnv n binding s@(h:t) = case findEnv n s of
                       Nothing -> Map.insert n binding h : t
                       Just _ -> snd $ mapAccumL f False s
  where
    f inserted env = if inserted
                     then (True, env)
                     else case Map.lookup n env of
                            Just _ -> (True, Map.insert n binding env)
                            Nothing -> (False, env)
putEnv _ _ _ = panic "putEnv: Empty env"
