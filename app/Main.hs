module Main where

import           Check
import           Error
import           Parser
import           Protolude

main :: IO ()
main = do
  file <- readFile "test.sml"
  let ast = parseString file
  result <- runErrWarn $ ast >>= typeCheck
  case result of
    Right x -> putStr (show x :: Text)
    Left e  -> putErr "Errors: " >> putErr (show e)

putErr :: Text -> IO ()
putErr = hPutStr stderr
