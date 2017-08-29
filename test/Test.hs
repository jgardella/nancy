import Test.Tasty
import Test.Tasty.HUnit
import Test.Unit.Typechecker
import Test.Unit.Interpreter

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ typecheckerTests
  , interpreterTests
  ]
