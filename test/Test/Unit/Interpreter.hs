module Test.Unit.Interpreter where

import Test.Tasty
import Test.Tasty.HUnit

import AudiComp.Core.Env as Env
import qualified Data.Map as Map
import AudiComp.Helper
import AudiComp.Interpreter
import AudiComp.Core.Language
import AudiComp.Core.Errors
import AudiComp.Core.Errors.Interpreter as InterpretE

assertRight msg result expected =
  case result of
    Right x -> assertEqual msg expected x
    Left err -> assertFailure ("Expected Right result but got " ++ show result)

assertLeft msg result expected =
  case result of
    Right x -> assertFailure ("Expected Left result but got " ++ show result)
    Left err -> assertEqual msg expected err

assertInterpretErr msg result expected =
  case result of
    Right x -> assertFailure ("Expected Left result but got " ++ show result)
    Left (InterpretErr err) -> assertEqual msg expected err
    err -> assertFailure ("Expected IntepretErr but got " ++ show err)

interpret = parseAndInterpret "<test>"

interpreterTests :: TestTree
interpreterTests = testGroup "Interpreter"
  [ intValue
  , boolValue
  ]

intValue =
  testGroup "Int"
  [
    testCase "int value" $
      assertRight ""
        (interpret "1")
        (IntV 1, ConstantIntW 1)
  ]

boolValue =
  testGroup "Bool"
  [
    testCase "true" $
      assertRight ""
        (interpret "true")
        (BoolV True, ConstantBoolW True)
  , testCase "false" $
      assertRight ""
        (interpret "false")
        (BoolV False, ConstantBoolW False)
  ]
