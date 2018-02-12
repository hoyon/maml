module Main where

import           Check
import           CodeGen
import qualified Data.ByteString.Lazy as BL
import           Error
import           WasmParse
import           Parser
import           Protolude

main :: IO ()
main = do
  file <- readFile "test.sml"
  let ast = parseString file
  let wasm = stdLib
  result <- runErrWarn wasm $ ast >>= typeCheck >>= codegen
  case result of
    Right x -> BL.putStr x
    Left e  -> putErr "Errors: " >> putErr (show e)

putErr :: Text -> IO ()
putErr = hPutStr stderr
