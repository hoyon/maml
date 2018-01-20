module ParsingSpec (spec) where
import           Check
import           Debug.Trace
import           Error
import           Parser
import           Protolude
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec

spec :: Spec
spec = do
  it "Integer value declaration" $
    parseAssert "val x = 1;"

  it "Negative number" $
    parseAssert "val x = ~1;"

  it "Double function" $
    parseAssert "fun double x = x * 2;"

  it "Add function" $
    parseAssert "fun add (x, y) = x + y;"

  it "Function call" $
    parseAssert "fun f (a, b) = add (a, b);"

  it "Undefined operand" $
    parseRefute "val x = 3 $ 4"

  where
    parseAssert = shouldSucceedOn $ parse lang ""
    parseRefute = shouldFailOn $ parse lang ""
