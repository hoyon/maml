module Main where

import           Check
import           CodeGen
import qualified Data.ByteString.Lazy as BL
import           Data.String
import           Error
import           Options.Applicative
import           Parser
import           Protolude
import           WasmParse

data Config = Config
  { input   :: String
  , output  :: String
  }

config :: Parser Config
config = Config
      <$> argument str
          ( metavar "INPUT"
         <> help "MaML source file to compile"
          )
      <*> strOption
          ( long "output"
         <> short 'o'
         <> help "Output file"
         <> value "out.wasm"
         <> metavar "OUTPUT"
          )

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (config <**> helper)
      ( fullDesc
     <> progDesc "Compile a MaML file"
      )

run :: Config -> IO ()
run config = do
  file <- readFile $ input config
  let ast = parseString file
  result <- runErrWarn stdLib $ ast >>= typeCheck >>= codegen
  case result of
    Right code -> BL.writeFile (output config) code
    Left e     -> putErr "Errors: " >> putErr (show e)

putErr :: Text -> IO ()
putErr = hPutStr stderr
