module CodeGen.Start where

import           CodeGen.Util
import           Data.Binary.Put
import           Protolude

startSectionCode :: Word8
startSectionCode = 8

genStart :: Int -> Put
genStart index = section startSectionCode $ putUleb128 index
