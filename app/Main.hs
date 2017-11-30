module Main where

import           Check
import           Error
import           Parser
import           Protolude
import           CodeGen
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
  file <- readFile "test.sml"
  let ast = parseString file
  result <- runErrWarn $ ast >>= typeCheck >>= codegen
  case result of
    Right x -> BL.putStr x
    Left e  -> putErr "Errors: " >> putErr (show e)

putErr :: Text -> IO ()
putErr = hPutStr stderr
