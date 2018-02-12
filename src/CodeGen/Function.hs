module CodeGen.Function where

import           AST
import           CodeGen.Global
import           CodeGen.Instruction
import           CodeGen.Util
import           Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import           Data.List            (elemIndex, lookup, (!!))
import qualified Data.Map             as Map
import           Env
import           Protolude            hiding (Infix, Type)
import           Type
import           WasmParse

typeSectionCode :: Word8
typeSectionCode = 1

functionSectionCode :: Word8
functionSectionCode = 3

codeSectionCode :: Word8
codeSectionCode = 10

data FunctionEntry = FunctionEntry { feIndex     :: Int
                                   , feName      :: Text
                                   , feParams    :: [Text]
                                   , feExpr      :: Expr
                                   , feTypeIndex :: Int
                                   }
                   | BuiltinFunctionEntry { bfeIndex     :: Int
                                          , bfeTypeIndex :: Int
                                          , bfeLocals    :: [LocalEntry]
                                          , bfeCode      :: [Word8]
                                          }
  deriving (Show)

type Locals = [Text]
data CompileConfig = CompileConfig { ccFunctions :: [FunctionEntry]
                                   , ccGlobals   :: GlobalMap
                                   , ccLocals    :: Locals
                                   }
type Compile = ReaderT CompileConfig PutM ()
runCompile :: CompileConfig -> Compile -> BL.ByteString
runCompile globals c = runPut $ runReaderT c globals


getFunctions :: Wasm -> Env -> ([TypeEntry], [FunctionEntry])
getFunctions wasm env = foldl f (builtinTypes, builtinFunctionEntries) (filter isFunction env)
  where
    -- Get builtin type entries and function entires
    exportedFunctions = filter (\e -> eeKind e == EkFunction) $ esEntries $ sectionData $ wasmExport wasm
    builtinFunctionIndices = fsEntries $ sectionData $ wasmFunction wasm
    builtinCodeEntries = csEntries $ sectionData $ wasmCode wasm
    builtinFunctionEntries = mapInd (\f i ->
                                       BuiltinFunctionEntry (eeIndex f)
                                       (builtinFunctionIndices !! i)
                                       (ceLocals $ builtinCodeEntries !! i)
                                       (ceCode $ builtinCodeEntries !! i)
                                    )
                              exportedFunctions

    builtinTypes = tsEntries $ sectionData $ wasmType wasm

    -- Map with index
    mapInd f l = zipWith f l [0..]

    isFunction (_, BdFun{}) = True
    isFunction _            = False

    makeTypeEntry (BdFun args _) = TypeEntry (map (const I32) args) (Just I32)
    makeFuncEntry name (BdFun args expr) idx typeIdx = FunctionEntry idx name args expr typeIdx

    f :: ([TypeEntry], [FunctionEntry]) -> (Text, Binding) -> ([TypeEntry], [FunctionEntry])
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
  case ret of
    Just t -> do
      putWord8 0x01 -- One return type
      putWord8 $ type2byte t
    Nothing -> putWord8 0x0 -- No return type

genSigs :: [FunctionEntry] -> Put
genSigs fs = section functionSectionCode $ do
  putUleb128 $ length fs
  putBytes $ concatMap (uleb128 . typeIndex) fs

  where
    typeIndex fe@FunctionEntry{}        = feTypeIndex fe
    typeIndex fe@BuiltinFunctionEntry{} = bfeTypeIndex fe

genCode :: [FunctionEntry] -> GlobalMap -> Put
genCode functions globals = section codeSectionCode $ do
  putUleb128 $ length functions
  mapM_ (functionEntry functions globals) functions

functionEntry :: [FunctionEntry] -> GlobalMap -> FunctionEntry -> Put
functionEntry functions globals fe = do
  putUleb128 $ fromIntegral $ BL.length body
  putLazyByteString body
  where
    body = runCompile (CompileConfig functions globals (feParams fe)) $
      case fe of
        FunctionEntry{} -> do
          lift $ putUleb128 0 -- no locals
          compile $ feExpr fe
          lift $ putWord8 0x0b
        BuiltinFunctionEntry{} -> do
          let locals = bfeLocals fe
          lift $ putUleb128 $ length locals
          mapM_ putLocalEntry locals
          lift $ putBytes $ bfeCode fe
          lift $ putWord8 0x0b

    putLocalEntry le = do
      lift $ putUleb128 $ leCount le
      lift $ putWord8 $ type2byte $ leType le

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
  case iden `elemIndex` ccLocals cc of
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
  case findFunc fname (ccFunctions cc) of
    Just fe -> do
      mapM_ compile args
      lift $ putWord8 0x10 -- call
      lift $ putUleb128 $ feIndex fe
    Nothing -> panic $ "Can't find function with name " <> fname
  where
    findFunc key [] = Nothing
    findFunc key (fe@(FunctionEntry _ name _ _ _):fs)
      | key == name = Just fe
      | otherwise = findFunc key fs
    findFunc key (_:fs) = findFunc key fs


compileOp :: Text -> Word8
compileOp op
  | op == "+" = i32Add
  | op == "-" = i32Sub
  | op == "*" = i32Mul
  | op == "/" = i32DivU

type2byte :: WasmType -> Word8
type2byte I32 = 0x7f
type2byte t   = panic $ "Can't gen this type yet: " <> show t
