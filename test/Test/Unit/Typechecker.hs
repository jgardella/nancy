module Test.Unit.Typechecker where

import Test.Tasty
import Test.Tasty.HUnit

import AudiComp.Typechecker
import AudiComp.Core.Language
import AudiComp.Helper

typecheck = parseAndTypecheck "<test>"

typecheckerTests :: TestTree
typecheckerTests = testGroup "Typechecker"
  [ intType
  , boolType
  , arrowType
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

arrowType =
  testGroup "Arrow"
  [
    testCase "Arrow type (constant body)" $
      assertEqual ""
        (typecheck "fun (x:int) -> true")
        (Right (ArrowT IntT BoolT, AbstractionW IntT (ConstantBoolW True)))
  , testCase "Arrow type (parameter usage)" $
      assertEqual ""
        (typecheck "fun (x:int) -> x")
        (Right (ArrowT IntT IntT, AbstractionW IntT (TruthHypothesisW IntT)))
  ]
