{-# LANGUAGE TemplateHaskell #-}
module WasmParse
  ( Section(..)
  , WasmType(..)
  , TypeEntry(..)
  , TypeSection(..)
  , FunctionSection(..)
  , GlobalEntry(..)
  , GlobalSection(..)
  , ExportKind(..)
  , ExportEntry(..)
  , ExportSection(..)
  , CodeEntry(..)
  , LocalEntry(..)
  , CodeSection(..)
  , VerbatimSection(..)
  , Wasm(..)
  , stdLib
  , stdLibExported
  )
where

import           Control.Monad
import           Control.Monad.Loops
import           Data.Binary.Get
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import           Data.FileEmbed
import           Data.Text.Encoding
import           Protolude            hiding (Type)

data Section s = Section
  { sectionId     :: Int
  , sectionLength :: Int
  , sectionData   :: s
  } | EmptySection
  deriving (Show)

data WasmType
  = I32
  | I64
  | F32
  | F64
  deriving (Show, Eq)

data TypeEntry = TypeEntry
  { teParams :: [WasmType]
  , teResult :: Maybe WasmType
  }
  deriving (Show, Eq)

newtype TypeSection = TypeSection
  { tsEntries :: [TypeEntry] }
  deriving (Show)

newtype FunctionSection = FunctionSection
  { fsEntries :: [Int] }
  deriving (Show)

data GlobalEntry = GlobalEntry
  { geType :: WasmType
  , geExpr :: [Word8] -- Includes end byte
  }
  deriving (Show)

newtype GlobalSection = GlobalSection
  { gsEntries :: [GlobalEntry] }
  deriving (Show)

data ExportKind
  = EkGlobal
  | EkFunction
  | EkMemory
  | EkTable
  deriving (Show, Eq)

data ExportEntry = ExportEntry
  { eeName  :: Text
  , eeKind  :: ExportKind
  , eeIndex :: Int
  }
  deriving (Show)

newtype ExportSection = ExportSection
  { esEntries :: [ExportEntry] }
  deriving (Show)

data CodeEntry = CodeEntry
  { ceLocals :: [LocalEntry]
  , ceCode   :: [Word8]
  }
  deriving (Show)

data LocalEntry = LocalEntry
  { leCount :: Int
  , leType  :: WasmType
  }
  deriving (Show)

newtype CodeSection = CodeSection
  { csEntries :: [CodeEntry] }
  deriving (Show)

-- | For sections to be copied verbatim
newtype VerbatimSection = VerbatimSection
  { vsCode :: [Word8] }
  deriving (Show)

data Wasm = Wasm
  { wasmType     :: Section TypeSection
  , wasmImport   :: Section VerbatimSection
  , wasmFunction :: Section FunctionSection
  , wasmTable    :: Section VerbatimSection
  , wasmMemory   :: Section VerbatimSection
  , wasmGlobal   :: Section GlobalSection
  , wasmExport   :: Section ExportSection
  , wasmStart    :: Section VerbatimSection
  , wasmElement  :: Section VerbatimSection
  , wasmCode     :: Section CodeSection
  , wasmData     :: Section VerbatimSection
  }
  deriving (Show)

parseWasm :: BL.ByteString -> Wasm
parseWasm = runGet parseWasm'

parseWasm' :: Get Wasm
parseWasm' = do
  checkMagic
  ts <- typeSection
  is <- verbatimSection 2
  fs <- functionSection
  tbs <- verbatimSection 4
  ms <- verbatimSection 5
  gs <- globalSection
  es <- exportSection
  ss <- verbatimSection 8
  els <- verbatimSection 9
  cs <- codeSection
  ds <- verbatimSection 11

  empty <- isEmpty
  if empty
    then return (Wasm ts is fs tbs ms gs es ss els cs ds)
    else fail "Expected end of input"

-- | Check magic bytes are correct
checkMagic :: Get ()
checkMagic = mapM_ checkByte expectedMagic
  where
    expectedMagic :: [Word8]
    expectedMagic = [0x0, 0x61, 0x73, 0x6d, 0x01, 0x0, 0x0, 0x0]

    checkByte :: Word8 -> Get ()
    checkByte b = do
      a <- getWord8
      when (a /= b) $ fail "Magic bytes invalid"

section :: Int -> (Int -> Get a) -> Get (Section a)
section expectedCode f = do
  empty <- isEmpty
  if not empty
    then do
      code <- fromIntegral <$> lookAhead getWord8
      if code == expectedCode
        then do
          skip 1 -- Skip looked ahead code byte
          size <- getUleb128
          contents <- f size
          return $ Section code size contents
        else
          return EmptySection
    else
      return EmptySection

typeSection :: Get (Section TypeSection)
typeSection = section 1 $ \_ -> do
  count <- getUleb128
  TypeSection <$> replicateM count typeEntry

typeEntry :: Get TypeEntry
typeEntry = do
  func <- getWord8
  unless (func == 0x60) (fail "Type must be func")
  paramCount <- getUleb128
  params <- replicateM paramCount getType
  resultCount <- getWord8
  unless (resultCount <= 0x01) (fail "Multiple return values not allowed")
  result <- replicateM (fromIntegral resultCount) getType
  return $ TypeEntry params (head result)

getType :: Get WasmType
getType = do
  byte <- getWord8
  case byte of
    0x7f -> return I32
    0x7e -> return I64
    0x7d -> return F32
    0x7c -> return F64
    _    -> fail "Unknown type"

verbatimSection :: Int -> Get (Section VerbatimSection)
verbatimSection code = section code $ \l ->
  VerbatimSection <$> replicateM l getWord8

functionSection :: Get (Section FunctionSection)
functionSection = section 3 $ \_ -> do
  count <- getUleb128
  FunctionSection <$> replicateM count getUleb128
globalSection :: Get (Section GlobalSection)
globalSection = section 6 $ \_ -> do
  count <- getUleb128
  GlobalSection <$> replicateM count globalEntry

globalEntry :: Get GlobalEntry
globalEntry = do
  t <- getType
  code <- getWord8 `untilM` (lookAhead getWord8 >>= (\x -> return $ x == 0x0b))
  skip 1 -- skip end byte
  return $ GlobalEntry t $ code ++ [0x0b]

exportSection :: Get (Section ExportSection)
exportSection = section 7 $ \_ -> do
  count <- getUleb128
  ExportSection <$> replicateM count exportEntry

exportEntry :: Get ExportEntry
exportEntry = do
  nameLength <- getUleb128
  nameBytes <- replicateM nameLength getWord8
  let name = decodeUtf8 $ BS.pack nameBytes
  kind <- exportKind
  index <- getUleb128
  return $ ExportEntry name kind index

exportKind :: Get ExportKind
exportKind = do
  kind <- getWord8
  case kind of
    0 -> return EkFunction
    1 -> return EkTable
    2 -> return EkMemory
    3 -> return EkGlobal
    _ -> fail "Unknown export kind"

codeSection :: Get (Section CodeSection)
codeSection = section 10 $ \_ -> do
  count <- getUleb128
  CodeSection <$> replicateM count codeEntry

codeEntry :: Get CodeEntry
codeEntry = do
  size <- getUleb128

  start <- bytesRead
  localCount <- getUleb128
  locals <- replicateM localCount localEntry
  end <- bytesRead

  let codeLength = size - fromIntegral (end - start) - 1

  code <- replicateM codeLength getWord8
  skip 1 -- skip end byte
  return $ CodeEntry locals code

localEntry :: Get LocalEntry
localEntry = do
  count <- getUleb128
  t <- getType
  return $ LocalEntry count t

-- | Decode a uleb128 value
getUleb128 :: Get Int
getUleb128 = getUleb128' 0 0
  where
    getUleb128' :: Int -> Int -> Get Int
    getUleb128' r s = do
      byte <- getWord8
      let a = r .|. fromIntegral ((byte .&. 0x7F) `shiftL` s)
      if (byte .&. 0x80) == 0
        then return a
        else getUleb128' a (s + 7)

stdLibRaw :: BS.ByteString
stdLibRaw = $(embedFile "lib/memory.wasm")

stdLib :: Wasm
stdLib = parseWasm (BL.fromStrict stdLibRaw)

stdLibExported :: [(Text, Int)]
stdLibExported = map (\e -> (eeName e, eeIndex e)) entries
  where
    entries = esEntries $ sectionData $ wasmExport stdLib
