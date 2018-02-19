module CodeGen.Expr where

import           AST
import           CodeGen.Types
import           Protolude hiding (Infix)
import           Data.Binary.Put
import           CodeGen.Instruction
import           CodeGen.Util
import           Data.List
import           WasmParse

runCompile :: CompileConfig -> Compile -> LByteString
runCompile globals c = runPut $ runReaderT c globals

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
          -- lift $ putWord8 0x23 -- get_global
          lift $ i32Const $ mgeIndex ge -- global index
          compile (CallBuiltin "get_global")
          lift $ putBytes i32Load
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

compile (CallBuiltin fname) = do
  lift $ putWord8 0x10 -- call
  lift $ putUleb128 index
  where
    index = case lookup fname stdLibExported of
      Just idx -> idx
      Nothing -> panic $ "Could not find built in function " <> fname

compileOp :: Text -> Word8
compileOp op
  | op == "+" = i32Add
  | op == "-" = i32Sub
  | op == "*" = i32Mul
  | op == "/" = i32DivU