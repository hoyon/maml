module Check (checkNames) where

import           AST
import           Data.List
import qualified Data.Set  as S
import qualified Data.Text as T

-- | Get the name of a declaration
name :: Dec -> T.Text
name (Val x _)   = x
name (Fun x _ _) = x

-- | Get all names defined in AST
names :: AST -> [T.Text]
names = fmap name

-- | The number of times each element appears in the list
numOccurances :: (Ord a, Eq a) => [a] -> [(Int, a)]
numOccurances xs = let set = S.fromList xs
                   in S.toList $ S.map (\x -> (length $ elemIndices x xs, x)) set

-- | Duplicate entries in list and the number of times they occur
duplicates :: [T.Text] -> [(Int, T.Text)]
duplicates xs = filter (\(x, _) -> x > 1) occurances
  where
    occurances = numOccurances xs

-- | Check for any names defined multiple times
checkNames :: AST -> Either T.Text AST
checkNames ast = if null dupes
                 then Right ast
                 else Left $ T.unlines $ map makeWarn dupes
  where
    dupes = duplicates $ names ast
    makeWarn (n, x) = T.concat ["name ", x, " defined ", T.pack . show $ n, " times"]
