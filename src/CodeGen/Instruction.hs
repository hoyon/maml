module CodeGen.Instruction where

import           CodeGen.Util
import           Data.Binary.Put
import           Protolude

i32Const :: Int -> Put
i32Const n = do
  putWord8 0x41 -- i32.const
  putBytes $ i32Literal n

-- sleb128 representation of number, with bounds checking
i32Literal :: Int -> [Word8]
i32Literal n
  | n >= min32 && n <= max32 = sleb128 n
  | otherwise = panic "Integer outside range for i32"
  where
    min32 = fromIntegral (minBound :: Int32)
    max32 = fromIntegral (maxBound :: Int32)

i32 :: Word8
i32 = 0x7f

-- 32 bit integer arithemetic operations
i32Add, i32Sub, i32Mul, i32DivU, i32RemS :: Word8
i32Add  = 0x6A
i32Sub  = 0x6B
i32Mul  = 0x6C
i32DivU = 0x6E
i32RemS = 0x6F

-- 32 bit integer comparison operators
i32Eq, i32Ne, i32Lt, i32Gt, i32Le, i32Ge :: Word8
i32Eq = 0x46
i32Ne = 0x47
i32Lt = 0x48
i32Gt = 0x4A
i32Le = 0x4C
i32Ge = 0x4E

-- 32 bit bitwise operators
i32And, i32Or, i32Xor :: Word8
i32And = 0x71
i32Or  = 0x72
i32Xor = 0x73

-- 32bit load and store
i32Load, i32Store :: [Word8]
i32Load = [0x28, 0x02, 0x00]
i32Store = [0x36, 0x02, 0x00]

i32If :: [Word8]
i32If = [0x04, i32]
