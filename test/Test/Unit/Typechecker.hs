module Test.Unit.Typechecker where

import Test.Tasty
import Test.Tasty.HUnit

import AudiComp.Typechecker
import AudiComp.Core.Language
import AudiComp.Helper

typecheck = parseAndTypecheck "<test>"

typecheckerTests :: TestTree
typecheckerTests = testGroup "Typechecker"
  [ intType ]

intType =
  testCase "int type" $
    assertEqual ""
      (typecheck "1")
      (Right (IntT, ConstantIntW 1))
