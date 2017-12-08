module CodeGen.Global ( genGlobal
                      , GlobalEntry(..)
                      , getGlobals
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

genGlobal :: Env -> Put
genGlobal env = section globalSectionCode $
  when (count > 0) $ do
    putBytes $ uleb128 count
    mapM_ globalEntry globals
  where
    globals = getGlobals env
    count = length globals

data GlobalEntry = GlobalEntry Text Type Constant Int
  deriving Show

-- | Get the entries which will be generated as globals
getGlobals :: Env -> [GlobalEntry]
getGlobals env = zipWith toGlobal (filter isGlobal env) [0..]
  where
    isGlobal (_, BdConst _ _) = True
    isGlobal _ = False

    toGlobal (name, BdConst tp c) n = GlobalEntry name tp c n
    toGlobal _ _ = notImplemented

globalEntry :: GlobalEntry -> Put
globalEntry (GlobalEntry _ TpInt (Number n) _) = do
  putWord8 0x7f -- i32
  putWord8 0x00 -- immutable
  i32Const n
  putWord8 0x0b -- end

globalEntry (GlobalEntry _ TpBool (Bool b) _) = do
  putWord8 0x7f -- i32
  putWord8 0x00 -- immutable
  i32Const $ if b then 1 else 0
  putWord8 0x0b -- end

globalEntry _ = notImplemented