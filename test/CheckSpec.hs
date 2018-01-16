module CheckSpec (spec) where
import           Check
import           Debug.Trace
import           Error
import           Parser
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec

spec :: Spec
spec = do
  describe "Value declarations" $ do
    it "Integer value declaraion" $
      checkProgram "val x = 1;"

    it "Dependent declarations" $
      checkProgram "val x = 1; val y = x;"

    it "Undefined value in declaration" $
      checkProgramFail "val x = y;"

    it "Name declared twice" $
      checkProgramFail "val x = 1; val x = 4;"

    it "Declaration expression" $
      checkProgram "val x = 1 + 4;"

    it "Declaration boolean" $
      checkProgramFail "val x = 1 == x;"

  describe "Function definitions" $ do
    it "Identity function" $
      checkProgram "fun f x = x;"

    it "Addition function" $
      checkProgram "fun f (a, b) = a + b;"

    it "Brackets in expression" $
      checkProgram "fun f (a, b, c) = (a + b) * c;"

    it "Returning a boolean expression" $
      checkProgramFail "fun f (a, b) = a == b;"

    it "Using a declared value in expression" $
      checkProgram "val x = 3; fun f y = x + y;"

    it "Using an undeclared value in expression" $
      checkProgramFail "fun f y = x + y;"

    it "Order doesn't matter" $
      checkProgram "fun f y = x + y; val x = 3;"

    it "If statement" $
      checkProgram "fun f (a, b) = if a == b then 1 else 0;"

    it "If statement wrong predicate" $
      checkProgramFail "fun f (a, b) = if a + b then 1 else 0;"

    it "Complex Predicate" $
      checkProgram "fun f (a, b, c, d) = if ((a == b) || (a == c)) && (c == d) then 1 else 0;"

    it "Function call" $
      checkProgram "fun f a = a + 3; fun g x = f (f x);"

    it "Wrong Arguments" $
      checkProgramFail "fun f (a, b) = a + b; fun g x = f x;"

    it "Factorial function" $
      checkProgram "fun fact n = if n == 1 then 1 else n * fact (n - 1);"

    it "Fibonacci function" $
      checkProgram "fun fib n = if n == 0 then 1 else if n == 1 then 1 else (fib (n - 1)) + (fib (n - 2));"

  where
    checkProgram str = do
      let ast = parseString str
      result <- runErrWarn $ ast >>= typeCheck
      result `shouldSatisfy` success

    checkProgram' str = do
      let ast = parseString str
      result <- runErrWarn $ ast >>= typeCheck
      (trace ("Env: " ++ show result)result) `shouldSatisfy` success

    checkProgramFail str = do
      let ast = parseString str
      result <- runErrWarn $ ast >>= typeCheck
      result `shouldSatisfy` fail

    success (Left _)  = False
    success (Right _) = True

    fail (Left _)  = True
    fail (Right _) = False
