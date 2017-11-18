module Main where

import           Check
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import           Parser
import           System.IO

main :: IO ()
main = do
  file <- T.readFile "test.sml"
  let ast = parseString file
  case ast >>= typeCheck of
    Right x -> putStr $ show x
    Left e  -> putErr "Errors: " >> putErr e

putErr :: T.Text -> IO ()
putErr = T.hPutStr stderr
