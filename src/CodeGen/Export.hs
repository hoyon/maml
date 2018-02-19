module CodeGen.Export ( genExport
                      ) where

import           CodeGen.Function
import           CodeGen.Start
import           CodeGen.Types
import           CodeGen.Util
import           Data.Binary.Put
import           Protolude

exportSectionCode :: Word8
exportSectionCode = 7

globalKind :: Word8
globalKind = 3

functionKind :: Word8
functionKind = 0

genExport :: GlobalMap -> [FunctionEntry] -> Put
genExport globals functions = section exportSectionCode $
  when (count > 0) $ do
    putUleb128 count
    -- mapM_ (\(iden, ge) -> exportEntry iden (mgeIndex ge) globalKind) globals
    mapM_ exportFunction functions

  where
    count = -- length globals +
      length (filter isExported functions)

    isExported FunctionEntry{}        = True
    isExported BuiltinFunctionEntry{} = False

exportFunction :: FunctionEntry -> Put
exportFunction fe@FunctionEntry{} = exportEntry (feName fe) (feIndex fe) functionKind
exportFunction BuiltinFunctionEntry{} = return ()

exportEntry
  :: Text   -- ^ Entry name
  -> Int    -- ^ Entry index
  -> Word8  -- ^ Entry kind
  -> Put
exportEntry name idx kind = do
  putString name
  putWord8 kind
  putUleb128 idx
