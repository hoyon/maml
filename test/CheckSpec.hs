module CheckSpec (spec) where
import           Check
import           Error
import           Parser
import           Protolude
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec
import           WasmParse

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
      checkProgram "val x = True;"

  describe "Function definitions" $ do
    it "Identity function" $
      checkProgram "fun f x = x;"

    it "Addition function" $
      checkProgram "fun f (a, b) = a + b;"

    it "Brackets in expression" $
      checkProgram "fun f (a, b, c) = (a + b) * c;"

    it "Returning a boolean expression" $
      checkProgram "fun f (a, b) = a == b;"

    it "Using a declared value in expression" $
      checkProgram "val x = 3; fun f y = x + y;"

    it "Using an undeclared value in expression" $
      checkProgramFail "fun f y = x + y;"

    it "Order matters" $ do
      checkProgramFail "fun f y = x + y; val x = 3;"
      checkProgramFail "val sum = a + b; val a = 3; val b = 2;"
      checkProgramFail "val sum = add 3 4; fun add (x, y) = x + y;"

    it "If statement" $
      checkProgram "fun f (a, b) = if a == b then 1 else 0;"

    it "If statement wrong predicate" $
      checkProgramFail "fun f (a, b) = if a + b then 1 else 0;"

    it "If statement different body types" $
      checkProgramFail "fun f a = if a == a then 1 else a != a;"

    it "Complex Predicate" $
      checkProgram "fun f (a, b, c, d) = if ((a == b) || (a == c)) && (c == d) then 1 else 0;"

    it "Function call" $
      checkProgram "fun f a = a + 3; fun g x = f x;"

    it "Nested function calls" $
      checkProgram "fun inc x = x + 1; fun inc2 x = inc (inc x);"

    it "Factorial function" $
      checkProgram "fun fact n = if n == 1 then 1 else n * fact (n - 1);"

    it "Fibonacci function" $
      checkProgram "fun fib n = if n == 0 then 1 else if n == 1 then 1 else (fib (n - 1)) + (fib (n - 2));"

    it "Bad function call" $
      checkProgramFail "fun add (a, b) = a + b; val n = add 3;"

    it "Good function call" $
      checkProgram "fun add (a, b) = a + b; val n = add 3 5;"

    it "Polymorphic id function" $
      checkProgram "fun id x = x; val i = id 1; val b = id True;"

  where
    checkProgram str = do
      let ast = parseString str
      result <- runErrWarn stdLib $ ast >>= typeCheck
      result `shouldSatisfy` success

    checkProgram' str = do
      let ast = parseString str
      result <- runErrWarn stdLib $ ast >>= typeCheck
      putStrLn $ "Env: " ++ show result
      result `shouldSatisfy` success

    checkProgramFail str = do
      let ast = parseString str
      result <- runErrWarn stdLib $ ast >>= typeCheck
      result `shouldSatisfy` fail

    success (Left _)  = False
    success (Right _) = True

    fail (Left _)  = True
    fail (Right _) = False
