{-# LANGUAGE OverloadedStrings #-}
module Main where

import Parser
import qualified Data.Text as T

main :: IO ()
main = do
  file <- readFile "test.sml"
  parseString $ T.pack file
