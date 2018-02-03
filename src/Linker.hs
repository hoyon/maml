{-# LANGUAGE TemplateHaskell #-}
module Linker where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.FileEmbed
import Linker.Parse

stdLib :: BS.ByteString
stdLib = $(embedFile "lib/memory.wasm")

stdLibWasm :: Wasm
stdLibWasm = parseWasm (BL.fromStrict stdLib)
