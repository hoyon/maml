module CodeGen.Export ( genExport
                      ) where

import           CodeGen.Function
import           CodeGen.Global
import           CodeGen.Util
import           Data.Binary.Put
import           Protolude

exportSectionCode :: Word8
exportSectionCode = 7

globalKind :: Word8
globalKind = 3

functionKind :: Word8
functionKind = 0

genExport :: GlobalMap -> FunctionMap -> Put
genExport globals functions = section exportSectionCode $
  when (count > 0) $ do
    putUleb128 count
    mapM_ (\(iden, ge) -> exportEntry iden (geIndex ge) globalKind) globals
    mapM_ (\(iden, fe) -> exportEntry iden (feIndex fe) functionKind) functions

  where
    count = length globals + length functions

exportEntry
  :: Text   -- ^ Entry name
  -> Int    -- ^ Entry index
  -> Word8  -- ^ Entry kind
  -> Put
exportEntry name idx kind = do
  putString name
  putWord8 kind
  putUleb128 idx
