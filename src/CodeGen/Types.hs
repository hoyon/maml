module CodeGen.Types where

import           AST
import           Protolude
import           Data.Binary.Put
import           WasmParse

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

data MamlGlobalEntry = MamlGlobalEntry
  { mgeIndex :: Int
  , mgeExpr  :: Expr
  }
  deriving Show

type GlobalMap = [(Text, MamlGlobalEntry)]
