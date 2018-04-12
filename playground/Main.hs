{-# LANGUAGE TemplateHaskell #-}

import Web.Scotty
import Protolude hiding (get)
import Data.FileEmbed
import Data.Monoid (mconcat)
import Data.String
import Data.Aeson (object, (.=))
import Data.ByteString.Base64.Lazy
import Network.HTTP.Types.Status

import CodeGen
import Check
import Error
import Parser
import WasmParse

indexFile = $(embedStringFile "playground/index.html")

main = scotty 3000 $ do
  get "/" showIndex
  post "/compile" compileProgram

showIndex :: ActionM ()
showIndex = do
  setHeader "Content-Type" "text/html"
  file "playground/index.html"
{-showIndex = text indexFile-}

compileProgram :: ActionM ()
compileProgram = do
  code <- param "code" `rescue` return
  let ast = parseString $ toS code
  res <- liftIO $ runErrWarn stdLib $ ast >>= typeCheck >>= codegen
  case res of
    Right bytes -> json $ object ["wasm" .= (toS $ encode bytes :: String)]
    Left err -> do
      status status400
      json $ object ["error" .= (toS $ formatError err :: String)]
