module CodeGen where

import           AST
import           CodeGen.Function
import           CodeGen.Global
import           CodeGen.Util
import           Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map             as Map
import           Env
import           Error
import           Protolude

-- | Magic bytes to start output
magic :: [Word8]
magic = [0x0, 0X61, 0x73, 0x6d]

-- | Version string
version :: [Word8]
version = [0x01, 0x0, 0x0, 0x0]

codegen :: Env -> ErrWarn LByteString
codegen e = return $ runPut $ codegen' e

codegen' :: Env -> Put
codegen' env = do
  putBytes magic
  putBytes version

  let globals = getGlobals env
  let (ts, fs) = getFunctions env

  genTypes ts
  genSigs fs
  genGlobal globals
  -- genExport globals fs
  genCode fs globals

