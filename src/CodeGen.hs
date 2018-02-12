module CodeGen where

import           AST
import           CodeGen.Export
import           CodeGen.Function
import           CodeGen.Global
import           CodeGen.Memory
import           CodeGen.Util
import           Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map             as Map
import           Env
import           Error
import           Protolude
import           WasmParse

-- | Magic bytes to start output
magic :: [Word8]
magic = [0x0, 0X61, 0x73, 0x6d]

-- | Version string
version :: [Word8]
version = [0x01, 0x0, 0x0, 0x0]

codegen :: Env -> ErrWarn LByteString
codegen e = do
  wasm <- ask
  return $ runPut $ codegen' e wasm

codegen' :: Env -> Wasm -> Put
codegen' env wasm = do
  putBytes magic
  putBytes version

  let globals = getGlobals env
  let (ts, fs) = getFunctions wasm env

  genTypes ts
  genSigs fs
  genMemory
  genGlobal globals
  genExport globals fs
  genCode fs globals

