module CodeGen.Global ( genGlobal
                      , GlobalEntry(..)
                      , getGlobals
                      , GlobalMap
                      ) where

import           AST
import           CodeGen.Instruction
import           CodeGen.Util
import           Data.Binary.Put
import           Env
import           Protolude
import           Type

-- | Section code for globals
globalSectionCode :: Word8
globalSectionCode = 6

-- | Get the entries which will be generated as globals
getGlobals :: Env -> GlobalMap
getGlobals env = zipWith toGlobal (filter isGlobal env) [0..]
  where
    isGlobal (_, BdConst _) = True
    isGlobal _ = False

    toGlobal (name, BdConst c) n = (name, GlobalEntry { index = n, value = c})
    toGlobal _ _ = notImplemented

genGlobal :: GlobalMap -> Put
genGlobal globals = section globalSectionCode $
  when (count > 0) $ do
    putBytes $ uleb128 count
    mapM_ globalEntry $ map snd globals
  where
    count = length globals

data GlobalEntry = GlobalEntry
  { index :: Int
  , value :: Constant
  }
  deriving Show

type GlobalMap = [(Text, GlobalEntry)]

globalEntry :: GlobalEntry -> Put
globalEntry (GlobalEntry _ (Number n)) = do
  putWord8 0x7f -- i32
  putWord8 0x00 -- immutable
  i32Const n
  putWord8 0x0b -- end

-- globalEntry (GlobalEntry _ TpBool (Bool b) _) = do
--   putWord8 0x7f -- i32
--   putWord8 0x00 -- immutable
--   i32Const $ if b then 1 else 0
--   putWord8 0x0b -- end

globalEntry _ = notImplemented
