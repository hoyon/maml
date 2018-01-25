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
import           System.IO (hGetContents)
import           Test.Hspec

spec :: Spec
spec = before_ compileWat $
  describe "Test programs" $ do
    programs <- runIO getPrograms
    mapM_ testCase programs

testCase :: FilePath -> SpecWith (Arg (IO ()))
testCase program = it program $ do
  let path = programDir <> program <> "/"

  mamlCode <- readFile (path <> program <> ".maml")
  compiled <- compile mamlCode

  target <- BL.readFile (path <> program <> ".wasm")

  compiled `shouldBe` target

compile :: Text -> IO BL.ByteString
compile program = do
  result <- runErrWarn $ parseString program >>= typeCheck >>= codegen
  case result of
    Right x -> return x
    Left e  -> throwIO (userError (show e))

programDir :: FilePath
programDir = "test/programs/"

getPrograms :: IO [FilePath]
getPrograms = listDirectory programDir

compileWat :: IO ()
compileWat = do
  programs <- getPrograms
  mapM_ runWat2Wasm programs

  where
    runWat2Wasm program = do
      let input = program <> ".wat"
      let output = program <> ".wasm"
      let dir = programDir <> program
      (_, _, Just herr, _) <- createProcess (proc "wat2wasm" [input, "-o", output]) { std_err = CreatePipe, cwd = Just dir}
      err <- hGetContents herr
      unless (null err) $ putStrLn $ "Error compiling " <> output <> ": \n" <> err

