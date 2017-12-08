{-# LANGUAGE TemplateHaskell #-}
module CodeGen.Function where

import           AST
import           CodeGen.Instruction
import           CodeGen.Util
import           Control.Lens
import           Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import           Data.List            (elemIndex)
import qualified Data.Map             as Map
import           Env
import           Protolude            hiding (Infix)
import           Type

typeSectionCode :: Word8
typeSectionCode = 1

functionSectionCode :: Word8
functionSectionCode = 3

codeSectionCode :: Word8
codeSectionCode = 10

data TypeEntry = TypeEntry [Type] Type
  deriving (Show, Eq)

data FunctionEntry = FunctionEntry { _name      :: Text
                                   , _params    :: [Text]
                                   , _expr      :: Expr
                                   , _typeIndex :: Int
                                   }
makeLenses ''FunctionEntry

getEntries :: Env -> ([TypeEntry], [FunctionEntry])
getEntries env = foldl f ([], []) (filter isFunction env)
  where
    isFunction (_, BdFun{}) = True
    isFunction _            = False

    makeTypeEntry (BdFun args ret _) = TypeEntry (map snd args) ret
    makeFuncEntry name (BdFun args _ expr) idx = FunctionEntry name (map fst args) expr idx

    f :: ([TypeEntry], [FunctionEntry]) -> (Text, Binding) -> ([TypeEntry], [FunctionEntry])
    f entries@(ts, fs) (name, binding) = let te = makeTypeEntry binding
                                             idx = elemIndex te ts
                                         in case idx of
                                             Just n -> over _2 (++ [makeFuncEntry name binding n]) entries
                                             Nothing -> (ts ++ [te], fs ++ [makeFuncEntry name binding (length ts)])

typeEntry :: TypeEntry -> Put
typeEntry (TypeEntry args ret) = do
  putWord8 0x60 -- Func type code
  putBytes $ uleb128 $ length args
  putBytes $ map type2byte args
  putWord8 0x01 -- One return type
  putWord8 $ type2byte ret
  where
    type2byte :: Type -> Word8
    type2byte TpInt  = 0x7f
    type2byte TpBool = 0x7f
    type2byte t      = panic $ "Can't gen this type yet: " <> show t

genTypes :: [TypeEntry] -> Put
genTypes ts = section typeSectionCode $ do
  putBytes $ uleb128 $ length ts
  mapM_ typeEntry ts

genSigs :: [FunctionEntry] -> Put
genSigs fs = section functionSectionCode $ do
  putBytes $ uleb128 $ length fs
  putBytes $ concatMap (\fn -> uleb128 $ fn^.typeIndex) fs

genCode :: [FunctionEntry] -> Put
genCode fs = section codeSectionCode $ do
  putBytes $ uleb128 $ length fs
  mapM_ functionEntry fs

functionEntry :: FunctionEntry -> Put
functionEntry fe = do
  putBytes $ uleb128 $ fromIntegral $ BL.length body
  putLazyByteString body
  where
    body = runPut $ do
      putBytes $ uleb128 0 -- no locals
      compile (fe^.expr)
      putWord8 0x0b

compile :: Expr -> Put
compile (Infix op a b) = do
  compile a
  compile b
  putWord8 $ compileOp op

compile (Con (Number n)) = i32Const n

compileOp :: Text -> Word8
compileOp op
  | op == "+" = i32Add
  | op == "-" = i32Sub
  | op == "*" = i32Mul
  | op == "/" = i32DivU
