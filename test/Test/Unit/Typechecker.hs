module Test.Unit.Typechecker where

import Test.Tasty
import Test.Tasty.HUnit

import AudiComp.Helper
import AudiComp.Typechecker
import AudiComp.Core.Language
import AudiComp.Core.Errors
import AudiComp.Core.Errors.Typechecker as TypecheckE

assertRight msg result expected =
  case result of
    Right x -> assertEqual msg expected x
    Left err -> assertFailure ("Expected Right result but got " ++ show result)

assertLeft msg result expected =
  case result of
    Right x -> assertFailure ("Expected Left result but got " ++ show result)
    Left err -> assertEqual msg expected err

assertTypecheckErr msg result expected =
  case result of
    Right x -> assertFailure ("Expected Left result but got " ++ show result)
    Left (TypecheckErr err) -> assertEqual msg expected err
    err -> assertFailure ("Expected TypecheckErr but got " ++ show err)

typecheck = parseAndTypecheck "<test>"

typecheckerTests :: TestTree
typecheckerTests = testGroup "Typechecker"
  [ intType
  , boolType
  , idType
  , arrowType
  , application
  ]

intType =
  testGroup "Int"
  [
    testCase "int type" $
      assertRight ""
        (typecheck "1")
        (IntT, ConstantIntW 1)
  ]

boolType =
  testGroup "Bool"
  [
    testCase "bool type (true)" $
      assertRight ""
        (typecheck "true")
        (BoolT, ConstantBoolW True)
  , testCase "bool type (false)" $
      assertRight ""
        (typecheck "false")
        (BoolT, ConstantBoolW False)
  ]

idType =
  testGroup "Id"
  [
    testCase "Lookup of undefined variable" $
      assertTypecheckErr ""
        (typecheck "x")
        (TypecheckE.TruthVarUndefined "x")
  ]

arrowType =
  testGroup "Arrow"
  [
    testCase "constant body" $
      assertRight ""
        (typecheck "fun (x:int) -> true")
        (ArrowT IntT BoolT, AbstractionW IntT (ConstantBoolW True))
  , testCase "parameter usage" $
      assertRight ""
        (typecheck "fun (x:int) -> x")
        (ArrowT IntT IntT, AbstractionW IntT (TruthHypothesisW IntT))
  ]

application =
  testGroup "Application"
  [
    testCase "function application" $
      assertRight ""
        (typecheck "(fun (x:int) -> true) 1")
        (BoolT,
          ApplicationW
            (AbstractionW IntT (ConstantBoolW True))
            (ConstantIntW 1))
  , testCase "function application with lookup" $
      assertRight ""
        (typecheck "(fun (x:int) -> x) 1")
        (IntT,
          ApplicationW
            (AbstractionW IntT (TruthHypothesisW IntT))
            (ConstantIntW 1))
  , testCase "expected arrow" $
    assertTypecheckErr ""
      (typecheck "1 true")
      (TypecheckE.ExpectedArrow (Number 1) IntT)
  , testCase "invalid arg type" $
    assertTypecheckErr ""
      (typecheck "(fun (x:int) -> true) true")
      (TypecheckE.InvalidArgType BoolT IntT)
  ]
