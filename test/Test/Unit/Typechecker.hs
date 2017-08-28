module Test.Unit.Typechecker where

import Test.Tasty
import Test.Tasty.HUnit

import AudiComp.Helper
import AudiComp.Typechecker
import AudiComp.Core.Language
import AudiComp.Core.Errors
import AudiComp.Core.Errors.Typechecker as TypecheckE

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
      assertEqual ""
        (typecheck "1")
        (Right (IntT, ConstantIntW 1))
  ]

boolType =
  testGroup "Bool"
  [
    testCase "bool type (true)" $
      assertEqual ""
        (typecheck "true")
        (Right (BoolT, ConstantBoolW True))
  , testCase "bool type (false)" $
      assertEqual ""
        (typecheck "false")
        (Right (BoolT, ConstantBoolW False))
  ]

idType =
  testGroup "Id"
  [
    testCase "Lookup of undefined variable" $
      assertEqual ""
        (typecheck "x")
        (Left (TypecheckErr (TypecheckE.TruthVarUndefined "x")))
  ]

arrowType =
  testGroup "Arrow"
  [
    testCase "constant body" $
      assertEqual ""
        (typecheck "fun (x:int) -> true")
        (Right (ArrowT IntT BoolT, AbstractionW IntT (ConstantBoolW True)))
  , testCase "parameter usage" $
      assertEqual ""
        (typecheck "fun (x:int) -> x")
        (Right (ArrowT IntT IntT, AbstractionW IntT (TruthHypothesisW IntT)))
  ]

application =
  testGroup "Application"
  [
    testCase "function application" $
      assertEqual ""
        (typecheck "(fun (x:int) -> true) 1")
        (Right (BoolT,
          ApplicationW
            (AbstractionW IntT (ConstantBoolW True))
            (ConstantIntW 1)))
  , testCase "function application with lookup" $
      assertEqual ""
        (typecheck "(fun (x:int) -> x) 1")
        (Right (IntT,
          ApplicationW
            (AbstractionW IntT (TruthHypothesisW IntT))
            (ConstantIntW 1)))
  , testCase "expected arrow" $
    assertEqual ""
      (typecheck "1 true")
      (Left $ TypecheckErr (TypecheckE.ExpectedArrow (Number 1) IntT))
  , testCase "invalid arg type" $
    assertEqual ""
      (typecheck "(fun (x:int) -> true) true")
      (Left $ TypecheckErr (TypecheckE.InvalidArgType BoolT IntT))
  ]
