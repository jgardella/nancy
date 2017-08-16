module AudiComp.Typechecker where

import Data.Function((&))
import Data.Either.Combinators
import AudiComp.Parser
import AudiComp.Core.Language as L
import Text.Printf
import AudiComp.Core.Env as E
import AudiComp.Core.Errors.Typechecker as Err
import AudiComp.Core.Util
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

type TypePair = (L.Type, L.Witness)

typecheckProgramEmptyEnvs :: Program -> Either String TypePair
typecheckProgramEmptyEnvs program =
  case typecheckProgram program E.empty E.empty E.empty of
    (Right x) -> Right x
    (Left x) -> Left $ prettyShow x

typecheckProgram :: Program -> Env L.Type -> Env L.Type -> Env L.Trail -> Either TypecheckerE TypePair
typecheckProgram (Program exp) =
  typecheckExpression exp

typecheckExpression :: Exp -> Env L.Type -> Env L.Type -> Env L.Trail -> Either TypecheckerE TypePair
typecheckExpression (Number n) _ _ _ =
  Right (L.IntT, L.ConstantIntW n)
typecheckExpression (Boolean b) _ _ _ =
  Right (L.BoolT, L.ConstantBoolW b)
typecheckExpression (Brack exp) tEnv wEnv eEnv =
  typecheckExpression exp tEnv wEnv eEnv
typecheckExpression (Id x) tEnv wEnv eEnv = do
  t <- E.loadE x (Err.TruthVarUndefined x) tEnv
  Right (t, L.TruthHypothesisW t)
typecheckExpression (Abs x t b) tEnv wEnv eEnv = do
  (returnType, returnProof) <- typecheckExpression b (E.save x t tEnv) wEnv eEnv
  Right (L.ArrowT t returnType, L.AbstractionW t returnProof)
typecheckExpression (App x y) tEnv wEnv eEnv = do
  (xType, xProof) <- typecheckExpression x tEnv wEnv eEnv
  case xType of
    (L.ArrowT l r) -> do
      (yType, yProof) <- typecheckExpression y tEnv wEnv eEnv
      if yType == l
      then Right (r, L.ApplicationW xProof yProof)
      else Left (Err.InvalidArgType yType l)
    t ->
      Left (Err.ExpectedArrow x xType)
typecheckExpression (AuditedVar trailRenames u) _ wEnv eEnv = do
  validityVar <- E.loadE u (Err.ValidityVarUndefined u) wEnv
  case validityVar of
    (L.BoxT _ trailEnv _ t) ->
      let initialTrailVars = E.keys eEnv
          boxTrailVars = E.keys trailEnv
          (domain, codomain) = unzipTrailRenames trailRenames
      in
        if codomain == initialTrailVars then
          if domain == boxTrailVars then
            Right (renameTypeTrailVars trailRenames t, L.ValidityHypothesisW u trailRenames)
          else
            Left (Err.InvalidRenameDomain boxTrailVars domain)
        else
            Left (Err.InvalidRenameCodomain initialTrailVars codomain)
    t -> Left (Err.ValidityVarWrongType u validityVar)
typecheckExpression (AuditedUnit trailVar exp) _ wEnv eEnv = do
  let newTrailEnv = E.save trailVar (L.Reflexivity $ L.TruthHypothesisW L.IntT) E.empty
  (expType, expProof) <- typecheckExpression exp E.empty wEnv newTrailEnv
  Right (L.BoxT trailVar (E.save trailVar (L.Reflexivity $ L.TruthHypothesisW L.IntT) E.empty) expProof expType, L.BoxIntroductionW newTrailEnv expProof)
typecheckExpression (AuditedComp u arg body) tEnv wEnv eEnv = do
  (argType, argProof) <- typecheckExpression arg tEnv wEnv eEnv
  case argType of
    (L.BoxT s trailEnv p t) -> do
      (bodyType, bodyProof) <- typecheckExpression body tEnv (E.save u argType wEnv) eEnv
      let subsitutedBodyType = subsituteTypeValidityVars ValidityVarSubParams{u=u, trailEnv=trailEnv, p=p} bodyType in
        Right (subsitutedBodyType, L.BoxEliminationW t s bodyProof argProof)
    t -> Left (Err.ExpectedBox argType)
typecheckExpression
  (TrailInspect trailVar
    (L.ReflexivityM exp_r)
    (L.SymmetryM s1 exp_s)
    (L.TransitivityM t1 t2 exp_t)
    (L.BetaM exp_ba)
    (L.BetaBoxM exp_bb)
    (L.TrailInspectionM exp_ti)
    (L.AbstractionM abs1 exp_abs)
    (L.ApplicationM app1 app2 exp_app)
    (L.LetM let1 let2 exp_let)
    (L.ReplacementM e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 exp_e))
    tEnv wEnv eEnv = do
  trail <- E.loadE trailVar (Err.TrailVarUndefined trailVar) eEnv
  (rType, rProof) <- typecheckExpression exp_r tEnv E.empty E.empty
  (sType, sProof) <- typecheckExpression exp_s (E.save s1 rType tEnv) E.empty E.empty
  (tType, tProof) <- typecheckExpression exp_t (E.save t2 rType (E.save t1 rType tEnv)) E.empty E.empty
  (baType, baProof) <- typecheckExpression exp_ba tEnv E.empty E.empty
  (bbType, bbProof) <- typecheckExpression exp_bb tEnv E.empty E.empty
  (tiType, tiProof) <- typecheckExpression exp_ti tEnv E.empty E.empty
  (absType, absProof) <- typecheckExpression exp_abs (E.save abs1 rType tEnv) E.empty E.empty
  (appType, appProof) <- typecheckExpression exp_app (E.save app1 rType (E.save app2 rType tEnv)) E.empty E.empty
  (letType, letProof) <- typecheckExpression exp_let (E.save let1 rType (E.save let2 rType tEnv)) E.empty E.empty
  (eType, eProof) <- typecheckExpression exp_e tEnv E.empty E.empty
  if allEqual [rType, sType, tType, baType, bbType, tiType, absType, appType, letType, eType] then
    Right (rType, L.TrailInspectionW trailVar rProof sProof tProof baProof bbProof tiProof absProof appProof letProof eProof)
  else Left Err.InconsistentTrailMappings
