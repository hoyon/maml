module CodeGen.Start where

import           AST
import           CodeGen.Expr
import           CodeGen.Function
import           CodeGen.Types
import           CodeGen.Util
import           Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import           Data.List            (elemIndex)
import           Env
import           Protolude
import           WasmParse

startSectionCode :: Word8
startSectionCode = 8

genStart :: Int -> Put
genStart index = section startSectionCode $ putUleb128 index

-- | Generate start function code. Still needs to be wrapped with metadata
makeStartFunction :: Env -> [FunctionEntry] -> [TypeEntry] -> FunctionEntry
makeStartFunction env functions types = BuiltinFunctionEntry
  { bfeIndex = length functions
  , bfeTypeIndex = typeIndex
  , bfeLocals = []
  , bfeCode = BL.unpack code
  }
  where
    globals = getGlobals env
    cc = CompileConfig{ ccFunctions = functions, ccGlobals = globals, ccLocals = []}
    callInit = runCompile cc (compile (CallBuiltin "init"))
    code = BL.concat $ callInit : map (\g -> startEntry (snd g) cc) globals
    typeIndex = case (TypeEntry [] Nothing) `elemIndex` types of
      Just idx -> idx
      Nothing -> panic "void type entry not found"

startEntry :: MamlGlobalEntry -> CompileConfig -> LByteString
startEntry g cc = runCompile cc $ do
  compile $ mgeExpr g
  compile (CallBuiltin "allocate_global_i32")
  lift $ putWord8 0x1a -- drop

getGlobals :: Env -> GlobalMap
getGlobals env = zipWith toGlobal (filter isGlobal env) [0..]
  where
    isGlobal (_, BdFun{}) = False
    isGlobal _            = True

    toGlobal (name, BdVal expr) n = (name, MamlGlobalEntry { mgeIndex = n, mgeExpr = expr})
    toGlobal _ _ = panic "Global binding must be a value"
