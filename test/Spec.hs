import           Parser
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec

main :: IO ()
main = hspec $ do
  describe "Parsing" $ do
    it "Value declaration" $
      parseAssert "val x = 1;"

    it "Tuple declaration" $
      parseAssert "val t = (1, 2);"

    it "Double function" $
      parseAssert "fun double x = x * 2;"

    it "Add function" $
      parseAssert "fun add (x, y) = x + y;"

    it "Fibonacci function" $
      parseAssert "fun fib n = if n == 1 then 1 else n * fib (n - 1);"

    it "Multiple args" $
      parseAssert "fun f (a, b, g) = g (a, b);"
  where
    parseAssert = shouldSucceedOn $ parse lang ""
