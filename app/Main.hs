module Main where

import           AST
import           Check
import qualified Data.Text as T
import           Parser
import           System.IO

main :: IO ()
main = do
  file <- readFile "test.sml"
  let ast = parseString $ T.pack file
  case ast >>= checkNames of
    Right x -> putStr $ showAST x
    Left e  -> putStrLn "Errors: " >> putErr e

putErr :: T.Text -> IO ()
putErr e = hPutStr stderr $ T.unpack e
