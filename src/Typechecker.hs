module Typechecker where

import Data.Function((&))
import Data.Either.Combinators
import Parser
import Language
import Types as T
import Text.Printf
import Env
import Util
import PreludeExtensions

type TypePair = (T.Type, T.Proof)

typecheckProgramEmptyEnvs :: Program -> Either String TypePair
typecheckProgramEmptyEnvs program =
  typecheckProgram program empty empty empty

typecheckProgram :: Program -> Env T.Type -> Env T.Type -> Env T.Trail -> Either String TypePair
typecheckProgram (Program exp) =
  typecheckExpression exp

typecheckExpression :: Exp -> Env T.Type -> Env T.Type -> Env T.Trail -> Either String TypePair
typecheckExpression (Number n) _ _ _ =
  Right (T.Int, T.ConstantInt n)
typecheckExpression (Boolean b) _ _ _ =
  Right (T.Bool, T.ConstantBool b)
typecheckExpression (Brack exp) tEnv pEnv eEnv =
  typecheckExpression exp tEnv pEnv eEnv
typecheckExpression (Id x) tEnv pEnv eEnv =
  load x tEnv
  & maybeToEither (printf "Truth variable %s not defined" x)
  & mapRight (\t -> (t, T.TruthHypothesis t))
typecheckExpression (Abs x t b) tEnv pEnv eEnv =
  typecheckExpression b (save x t tEnv) pEnv eEnv
  & mapRight (\(returnType, returnProof) -> (T.Arrow t returnType, T.Abstraction t returnProof))
typecheckExpression (App x y) tEnv pEnv eEnv =
  typecheckExpression x tEnv pEnv eEnv
  & bindRight (\(xType, xProof) ->
    case xType of
      (T.Arrow l r) ->
        typecheckExpression y tEnv pEnv eEnv
        & bindRight (\(yType, yProof) ->
            if yType == l
            then Right (r, T.Application xProof yProof)
            else Left (printf "Expected type of function '%s' does not match given type '%s'" (pretty l) (pretty yType)))
      t ->
        Left (printf "Left expression of App '%s' has type %s, should have type Arrow\n" (show x) (pretty t)))
typecheckExpression (AuditedVar u oldTrailVar newTrailVar) _ pEnv eEnv =
  load u pEnv
  & maybeToEither (printf "Validity variable %s is not defined" u)
  & bindRight (\validityVar ->
    case validityVar of
      (T.Audited t) ->
        Right (renameTypeTrailVars RenameTrailVarsParams{old=oldTrailVar, new=newTrailVar} t, T.ValidityHypothesis u oldTrailVar newTrailVar)
      t -> Left (printf "Validity variable %s has type %s, should have type Audited" u (pretty t)))
typecheckExpression (AuditedUnit trailVar exp) _ pEnv eEnv =
  typecheckExpression exp empty pEnv (save trailVar (T.Reflexivity $ T.TruthHypothesis T.Int) eEnv)
  & mapRight (\(expType, expProof) -> (T.Box eEnv expProof expType, T.BoxIntroduction eEnv expProof))
typecheckExpression (AuditedComp u arg body) tEnv pEnv eEnv =
  typecheckExpression arg tEnv pEnv eEnv
  & bindRight (\(argType, argProof) ->
    case argType of
      (T.Box trailEnv p t) ->
        typecheckExpression body tEnv (save u (T.Audited t) pEnv) eEnv
        & mapRight (\(bodyType, bodyProof) ->
          let subsitutedBodyType = subsituteTypeValidityVars ValidityVarSubParams{u=u, trailEnv=trailEnv, p=p} bodyType in
          (subsitutedBodyType, T.BoxElimination t bodyProof argProof))
      t -> Left (printf "Audited composition 'be' has type %s, should have Type Box" (pretty t)))
typecheckExpression
  (TrailInspect trailVar
    (Language.Reflexivity exp_r)
    (Language.Symmetry s1 exp_s)
    (Language.Transitivity t1 t2 exp_t)
    (Language.Beta exp_ba)
    (Language.BetaBox exp_bb)
    (Language.TrailInspection exp_ti)
    (Language.Abstraction abs1 exp_abs)
    (Language.Application app1 app2 exp_app)
    (Language.Let let1 let2 exp_let)
    (Language.Replacement e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 exp_e))
    tEnv pEnv eEnv =
  load trailVar eEnv
  & maybeToEither (printf "Trail variable %s is not defined" trailVar)
  & bindRight (\trail -> do
    (rType, rProof) <- typecheckExpression exp_r tEnv empty empty
    (sType, sProof) <- typecheckExpression exp_s (save s1 rType tEnv) empty empty
    (tType, tProof) <- typecheckExpression exp_t (save t2 rType (save t1 rType tEnv)) empty empty
    (baType, baProof) <- typecheckExpression exp_ba tEnv empty empty
    (bbType, bbProof) <- typecheckExpression exp_bb tEnv empty empty
    (tiType, tiProof) <- typecheckExpression exp_ti tEnv empty empty
    (absType, absProof) <- typecheckExpression exp_abs (save abs1 rType tEnv) empty empty
    (appType, appProof) <- typecheckExpression exp_app (save app1 rType (save app2 rType tEnv)) empty empty
    (letType, letProof) <- typecheckExpression exp_let (save let1 rType (save let2 rType tEnv)) empty empty
    (eType, eProof) <- typecheckExpression exp_e tEnv empty empty
    if allEqual [rType, sType, tType, baType, bbType, tiType, absType, appType, letType, eType] then
      Right (rType, T.TrailInspectionP trailVar rProof sProof tProof baProof bbProof tiProof absProof appProof letProof eProof)
    else Left (printf "All trail mappings should have same type"))
