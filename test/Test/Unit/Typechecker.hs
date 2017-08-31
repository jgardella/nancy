module Test.Unit.Typechecker where

import Test.Tasty
import Test.Tasty.HUnit

import Nancy.Core.Env as Env
import qualified Data.Map as Map
import Nancy.Helper
import Nancy.Typechecker
import Nancy.Core.Language
import Nancy.Core.Errors
import Nancy.Core.Errors.Typechecker as TypecheckE

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
typecheckWithEnv = parseAndTypecheckWithEnv "<test>"

typecheckerTests :: TestTree
typecheckerTests = testGroup "Typechecker"
  [ intType
  , boolType
  , idType
  , arrowType
  , application
  , auditedVar
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
    testCase "undefined truth variable" $
      assertTypecheckErr ""
        (typecheck "x")
        (TypecheckE.TruthVarUndefined "x")
  , testCase "defined truth variable" $
      assertRight ""
        (typecheckWithEnv "x"
          ((Env.save "x" IntT Env.empty),
          (Env.empty),
          (Env.empty))
        )
        (IntT, TruthHypothesisW IntT)
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

auditedVar =
  testGroup "Audited Computation"
  [
    testCase "undefined validity variable" $
      assertTypecheckErr ""
        (typecheck "<x->y|z>")
        (TypecheckE.ValidityVarUndefined "z")
  , testCase "audited unit" $
      assertRight ""
        (typecheck "!u true")
        (
          (BoxT "u"
            (Map.fromList [("u", (Reflexivity $ TruthHypothesisW IntT))])
            (ConstantBoolW True)
            BoolT),
          BoxIntroductionW
            (Map.fromList [("u", (Reflexivity $ TruthHypothesisW IntT))])
            (ConstantBoolW True)
        )
  , testCase "audited composition" $
      assertRight ""
        (typecheck
         "!x\
          \ let u:int->bool be\
          \   !y fun (a:int) -> true\
          \ in\
          \   <y->x | u> 3")
        ((BoxT
          "x"
          (Map.fromList [("x", Reflexivity (TruthHypothesisW IntT))])
          (BoxEliminationW "y"
            (ArrowT IntT BoolT)
            (ApplicationW
              (ValidityHypothesisW "u" [TrailRename {old = "y", new = "x"}])
              (ConstantIntW 3))
            (BoxIntroductionW
              (Map.fromList [("y", Reflexivity (TruthHypothesisW IntT))])
              (AbstractionW IntT (ConstantBoolW True))))
          BoolT),
        (BoxIntroductionW
          (Map.fromList [("x", Reflexivity (TruthHypothesisW IntT))])
          (BoxEliminationW "y"
            (ArrowT IntT BoolT)
            (ApplicationW
               (ValidityHypothesisW "u" [TrailRename {old = "y", new = "x"}])
               (ConstantIntW 3))
            (BoxIntroductionW
              (Map.fromList [("y", Reflexivity (TruthHypothesisW IntT))])
              (AbstractionW IntT (ConstantBoolW True))))))
  , testCase "invalid rename domain" $
      assertTypecheckErr ""
        (typecheck
         "!x\
          \ let u:int->bool be\
          \   !y fun (a:int) -> true\
          \ in\
          \   <z->x | u> 3")
        (TypecheckE.InvalidRenameDomain ["y"] ["z"])
  , testCase "invalid rename codomain" $
      assertTypecheckErr ""
        (typecheck
         "!x\
          \ let u:int->bool be\
          \   !y fun (a:int) -> true\
          \ in\
          \   <y->z | u> 3")
        (TypecheckE.InvalidRenameCodomain ["x"] ["z"])
  , testCase "trail inspection" $
      assertRight ""
        (typecheck
         "!e (\
          \ e[\
          \   r -> false;\
          \   s(x) -> x;\
          \   t(x y) ->  x;\
          \   ba -> true;\
          \   bb -> true;\
          \   ti -> false;\
          \   abs(x) -> x;\
          \   app(x y) -> x;\
          \   let(x y) -> x;\
          \   trpl(x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) -> true\
          \   ]\
          \  )")
        ((BoxT "e"
          (Map.fromList [("e",Reflexivity (TruthHypothesisW IntT))])
          (TrailInspectionW "e"
            (ConstantBoolW False)
            (TruthHypothesisW BoolT)
            (TruthHypothesisW BoolT)
            (ConstantBoolW True)
            (ConstantBoolW True)
            (ConstantBoolW False)
            (TruthHypothesisW BoolT)
            (TruthHypothesisW BoolT)
            (TruthHypothesisW BoolT)
            (ConstantBoolW True))
          BoolT),
        (BoxIntroductionW
          (Map.fromList [("e",Reflexivity (TruthHypothesisW IntT))])
          (TrailInspectionW "e"
            (ConstantBoolW False)
            (TruthHypothesisW BoolT)
            (TruthHypothesisW BoolT)
            (ConstantBoolW True)
            (ConstantBoolW True)
            (ConstantBoolW False)
            (TruthHypothesisW BoolT)
            (TruthHypothesisW BoolT)
            (TruthHypothesisW BoolT)
            (ConstantBoolW True))))
  ]
