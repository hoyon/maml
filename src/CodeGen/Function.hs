{-# LANGUAGE TemplateHaskell #-}
module CodeGen.Function where

import           AST
import           CodeGen.Instruction
import           CodeGen.Util
import           CodeGen.Global
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

getFunctions :: Env -> ([TypeEntry], [FunctionEntry])
getFunctions env = foldl f ([], []) (filter isFunction env)
  where
    isFunction (_, BdFun{}) = True
    isFunction _            = False

    makeTypeEntry (BdFun args _) = TypeEntry (map (const TpInt) args) TpInt
    makeFuncEntry name (BdFun args expr) idx = FunctionEntry name args expr idx

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

genCode :: [FunctionEntry] -> GlobalMap -> Put
genCode fs globals = section codeSectionCode $ do
  putBytes $ uleb128 $ length fs
  mapM_ (flip functionEntry globals) fs

type Compile = ReaderT GlobalMap PutM ()

runCompile :: GlobalMap -> Compile -> BL.ByteString
runCompile globals c = runPut $ flip runReaderT globals c

functionEntry :: FunctionEntry -> GlobalMap -> Put
functionEntry fe globals = do
  putBytes $ uleb128 $ fromIntegral $ BL.length body
  putLazyByteString body
  where
    body = runCompile globals $ do
      lift $ putBytes $ uleb128 $ length (fe^.params)
      compile (fe^.expr)
      lift $ putWord8 0x0b

compile :: Expr -> Compile
compile (Infix op a b) = do
  compile a
  compile b
  lift $ putWord8 $ compileOp op

compile (Con (Number n)) = lift $ i32Const n

compileOp :: Text -> Word8
compileOp op
  | op == "+" = i32Add
  | op == "-" = i32Sub
  | op == "*" = i32Mul
  | op == "/" = i32DivU
