module CodeGen.Util ( section
                    , uleb128
                    , sleb128
                    , encodeString
                    , putBytes
                    , putUleb128
                    , putSleb128
                    ) where

import           Data.Binary.Put
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Text.Encoding
import           Protolude

-- | Framework for section
section :: Word8 -> Put -> Put
section code content =
  when (count > 0) $ do -- Only emit section if it isn't empty
    putWord8 code
    putUleb128 $ fromIntegral $ BL.length contents
    putLazyByteString contents
  where
    contents = runPut content
    count = BL.length contents

-- | Unsigned uleb128 encoder
uleb128 :: Int -> [Word8]
uleb128 n
  | n < 0 = panic "uleb128 cannot be called with a negative number"
  | value == 0 = [byte]
  | otherwise = (byte .|. 0x80) : uleb128 value
  where
    byte = fromIntegral (n .&. 0x7F)
    value = n `shiftR` 7

-- | Signed uleb128 encoder
sleb128 :: Int -> [Word8]
sleb128 n
  | end = [byte]
  | otherwise = (byte .|. 0x80) : sleb128 value
  where
    byte = fromIntegral (n .&. 0x7F)
    value = n `shiftR` 7
    sb = byte .&. 0x40 == 0x40
    end = (value == 0 && not sb) || (value == -1 && sb)

-- | Encode a string in wasm format
encodeString :: Text -> [Word8]
encodeString t = uleb128 (length bytes) ++ bytes
  where
    bytes = BS.unpack $ encodeUtf8 t

-- | Put a series of Bytes
putBytes :: [Word8] -> Put
putBytes = mapM_ putWord8

-- | Put a number in uleb128 format
putUleb128 :: Int -> Put
putUleb128 = putBytes . uleb128

-- | Put a number in sleb128 format
putSleb128 :: Int -> Put
putSleb128 = putBytes . sleb128
