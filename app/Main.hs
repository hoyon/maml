module Main where

import           AST
import qualified Data.Text as T
import           Parser

main :: IO ()
main = do
  file <- readFile "test.sml"
  case parseString $ T.pack file of
    Right x -> putStrLn $ showDec x
    Left e  -> putStrLn $ T.unpack e
