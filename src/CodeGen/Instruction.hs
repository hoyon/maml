module CodeGen.Instruction ( i32Const
                           , i32Add
                           , i32Sub
                           , i32Mul
                           , i32DivU
                           )where

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

i32Add, i32Sub, i32Mul, i32DivU :: Word8
i32Add = 0x6A
i32Sub = 0x6B
i32Mul = 0x6C
i32DivU = 0x6E
