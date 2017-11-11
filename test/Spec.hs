import Parser
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

main :: IO ()
main = hspec $ do
  describe "Parsing" $ do
    it "Value declaration" $
      parseLang `shouldSucceedOn` "val x = 1;"
    it "Double function" $
      parseLang `shouldSucceedOn` "fun double x = x * 2;"
    it "Add function" $
      parseLang `shouldSucceedOn` "fun add (x, y) = x + y;"
    it "Fibonacci function" $
      parseLang `shouldSucceedOn` "fun fib n = if n == 1 then 1 else n * fib (n - 1);"
  where
    parseLang = parse lang ""
