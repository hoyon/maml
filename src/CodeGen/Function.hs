module CodeGen.Function where

import           AST
import           CodeGen.Instruction
import           CodeGen.Util
import           CodeGen.Global
import           Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import           Data.List            (elemIndex, lookup)
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

data FunctionEntry = FunctionEntry { feIndex     :: Int
                                   , feParams    :: [Text]
                                   , feExpr      :: Expr
                                   , feTypeIndex :: Int
                                   }
  deriving (Show)

type FunctionMap = [(Text, FunctionEntry)]

type Locals = [Text]
data CompileConfig = CompileConfig { ccFunctions :: FunctionMap
                                   , ccGlobals   :: GlobalMap
                                   , ccLocals    :: Locals
                                   }
type Compile = ReaderT CompileConfig PutM ()
runCompile :: CompileConfig -> Compile -> BL.ByteString
runCompile globals c = runPut $ flip runReaderT globals c


getFunctions :: Env -> ([TypeEntry], FunctionMap)
getFunctions env = foldl f ([], []) (filter isFunction env)
  where
    isFunction (_, BdFun{}) = True
    isFunction _            = False

    makeTypeEntry (BdFun args _) = TypeEntry (map (const TpInt) args) TpInt
    makeFuncEntry name (BdFun args expr) idx typeIdx = (name, FunctionEntry idx args expr typeIdx)

    f :: ([TypeEntry], FunctionMap) -> (Text, Binding) -> ([TypeEntry], FunctionMap)
    f (ts, fs) (name, binding) = let te = makeTypeEntry binding
                                     tIdx = elemIndex te ts
                                 in case tIdx of
                                      Just n -> (ts, fs ++ [makeFuncEntry name binding (length fs) n])
                                      Nothing -> (ts ++ [te],
                                                   fs ++ [makeFuncEntry name binding (length fs) (length ts)])


genTypes :: [TypeEntry] -> Put
genTypes ts = section typeSectionCode $ do
  putUleb128 $ length ts
  mapM_ typeEntry ts

typeEntry :: TypeEntry -> Put
typeEntry (TypeEntry args ret) = do
  putWord8 0x60 -- Func type code
  putUleb128 $ length args
  putBytes $ map type2byte args
  putWord8 0x01 -- One return type
  putWord8 $ type2byte ret
  where
    type2byte :: Type -> Word8
    type2byte TpInt  = 0x7f
    type2byte TpBool = 0x7f
    type2byte t      = panic $ "Can't gen this type yet: " <> show t

genSigs :: FunctionMap -> Put
genSigs fs = section functionSectionCode $ do
  putUleb128 $ length fs
  putBytes $ concatMap (\fn -> uleb128 $ feTypeIndex $ snd fn) fs

genCode :: FunctionMap -> GlobalMap -> Put
genCode functions globals = section codeSectionCode $ do
  putUleb128 $ length functions
  mapM_ (functionEntry functions globals) functions

functionEntry :: FunctionMap -> GlobalMap -> (Text, FunctionEntry) -> Put
functionEntry functions globals (_, fe)= do
  putUleb128 $ fromIntegral $ BL.length body
  putLazyByteString body
  where
    body = runCompile (CompileConfig functions globals (feParams fe)) $ do
      lift $ putUleb128 0 -- no locals
      compile $ feExpr fe
      lift $ putWord8 0x0b

-- | Main expression compiling function
compile :: Expr -> Compile
compile (Infix op a b) = do
  compile a
  compile b
  lift $ putWord8 $ compileOp op

compile (Con (Number n)) = lift $ i32Const n

compile (Id iden) = do
  cc <- ask
  -- Check local scope, then global scope
  case iden `elemIndex` (ccLocals cc) of
    Just idx -> do -- Use Local
      lift $ putWord8 0x20 -- get_local
      lift $ putUleb128 idx -- local index
    Nothing -> -- Use global
      case lookup iden (ccGlobals cc) of
        Just ge -> do
          lift $ putWord8 0x23 -- get_global
          lift $ putUleb128 $ geIndex ge -- global index
        Nothing -> panic $ "Can't find binding for " <> iden

compile (Call fname args) = do
  cc <- ask
  case lookup fname (ccFunctions cc) of
    Just fe -> do
      mapM_ (\arg -> compile arg) args
      lift $ putWord8 0x10 -- call
      lift $ putUleb128 $ feIndex fe
    Nothing -> panic $ "Can't find function with name " <> fname


compileOp :: Text -> Word8
compileOp op
  | op == "+" = i32Add
  | op == "-" = i32Sub
  | op == "*" = i32Mul
  | op == "/" = i32DivU
