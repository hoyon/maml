module CodeGen.Memory ( genMemory ) where

import           CodeGen.Util
import           Data.Binary.Put
import           Protolude

memorySectionCode :: Word8
memorySectionCode = 5

genMemory :: Put
genMemory = section memorySectionCode $ do
  putUleb128 1 -- Only one memory allows in MVP

  putWord8 0 -- Don't specify maximum memory size
  putUleb128 1 -- Use 1 page of memory (64KB)
