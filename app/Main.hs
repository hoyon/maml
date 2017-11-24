module Main where

import           Check
import           Error
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import           Parser
import           System.IO

main :: IO ()
main = do
  file <- T.readFile "test.sml"
  let ast = parseString file
  result <- runErrWarn $ ast >>= typeCheck
  case result of
    Right x -> putStr $ show x
    Left e  -> putErr "Errors: " >> putErr (T.pack $ show e)

putErr :: T.Text -> IO ()
putErr = T.hPutStr stderr
