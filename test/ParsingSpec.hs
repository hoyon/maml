module ParsingSpec (spec) where
import           Check
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

  it "Single argument function call" $
    parseAssert "fun f x = id x;"

  it "Multiple argument function call" $
    parseAssert "fun f (a, b) = add (a, b);"

  it "Missing semicolon" $
    parseRefute "val x = 3 val y = 2"

  it "If statement" $
    parseAssert "fun f x = if x == 3 then 1 else 0;"

  it "Missing else branch in if statement" $
    parseRefute "fun f x = if x == 3 then 1;"

  it "Parenthesis in expression" $
    parseAssert "fun f (x, y, z) = (x + y) * z;"

  it "Undefined operand" $
    parseRefute "val x = 3 $ 4"

  it "Unknown top level binding" $
    parseRefute "var x = 3;"

  it "Use reserved word as function name" $
    parseRefute "fun if x = x;"

  it "Use reserved word in expression" $
    parseRefute "fun f x = if + x;"

  it "Variable name starting with reserved word" $
    parseAssert "val if1 = 3;"

  it "Function name starting with reserved word" $
    parseAssert "fun if1 n = n + 1"

  it "Underscore in variable name" $
    parseAssert "val a_a = 4;"

  it "Underscore variable name" $
    parseAssert "val _ = 2;"

  it "Bad variable name" $
    parseRefute "val $ = 5;"

  where
    parseAssert = shouldSucceedOn $ parse lang ""
    parseRefute = shouldFailOn $ parse lang ""
