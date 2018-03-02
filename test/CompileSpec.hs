module CompileSpec (spec) where
import           Check
import           CodeGen
import qualified Data.ByteString.Lazy as BL
import           Error
import           Parser
import           Protolude
import           System.Directory
import           System.IO.Error
import           System.Process
import           System.IO (hClose, hGetContents, openBinaryTempFile)
import           Test.Hspec
import           WasmParse

spec :: Spec
spec = beforeAll checkWasmBinary $
  describe "Test programs" $ do
    programs <- runIO getPrograms
    mapM_ testCase programs

testCase :: FilePath -> SpecWith (Arg (IO ()))
testCase program = it program $ do
  let path = programDir <> program <> "/"

  -- Compile maml
  mamlCode <- readFile (path <> program <> ".maml")
  compiled <- compile mamlCode

  -- Save compiled wasm to temp file
  (tmpPath, tmpHandle) <- openBinaryTempFile "/tmp" (program <> ".wasm")
  BL.hPut tmpHandle compiled
  hClose tmpHandle

  output <- runWasmTest tmpPath (path <> program <> ".wast") path

  -- Delete tmp file
  removeFile tmpPath

  -- No output if tests succeeded
  output `shouldBe` ""

runWasmTest
  :: FilePath -- Wasm file
  -> FilePath -- Wast test file
  -> FilePath -- Working directory
  -> IO (Text) -- Pass or fail
runWasmTest wasm wast dir = do
  (_, _, Just herr, p) <- createProcess (proc "wasm" [wasm, "-i", wast]) { std_err = CreatePipe }
  waitForProcess p
  toS <$> hGetContents herr

compile :: Text -> IO LByteString
compile program = do
  result <- runErrWarn stdLib $ parseString program >>= typeCheck >>= codegen
  case result of
    Right x -> return x
    Left e  -> throwIO (userError (show e))

programDir :: FilePath
programDir = "test/programs/"

getPrograms :: IO [FilePath]
getPrograms = listDirectory programDir

checkWasmBinary :: IO ()
checkWasmBinary = do
  wasm <- findExecutable "wasm"
  wasm `shouldNotBe` Nothing
