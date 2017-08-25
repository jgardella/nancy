module AudiComp.Typechecker where

import Data.Function((&))
import Data.Either.Combinators
import AudiComp.Parser
import AudiComp.Core.Language as L
import Text.Printf
import AudiComp.Core.Env as E
import AudiComp.Core.Errors.Typechecker as Err
import AudiComp.Core.Util
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

type TypePair = (L.Type, L.Witness)

type TypecheckEnv = (Env L.Type, Env L.Type, Env L.Trail)

type TypecheckM = ReaderT TypecheckEnv (ExceptT Err.TypecheckerE Identity) TypePair

runTypecheckM :: TypecheckEnv -> TypecheckM -> Either Err.TypecheckerE TypePair
runTypecheckM env m = runIdentity (runExceptT (runReaderT m env))

updateTruthEnv :: (Env L.Type -> Env L.Type) -> TypecheckEnv -> TypecheckEnv
updateTruthEnv f (tEnv, wEnv, eEnv) =
  (f tEnv, wEnv, eEnv)

updateWitnessEnv :: (Env L.Type -> Env L.Type) -> TypecheckEnv -> TypecheckEnv
updateWitnessEnv f (tEnv, wEnv, eEnv) =
  (tEnv, f wEnv, eEnv)

typecheckProgramEmptyEnvs :: Program -> Either String TypePair
typecheckProgramEmptyEnvs program =
  case typecheckProgram (E.empty, E.empty, E.empty) program of
    (Right x) -> Right x
    (Left x) -> Left $ prettyShow x

typecheckProgram :: TypecheckEnv -> Program -> Either Err.TypecheckerE TypePair
typecheckProgram env (Program exp) =
  runTypecheckM env (typecheckExpression exp)

typecheckExpression :: Exp -> TypecheckM
typecheckExpression (Number n) =
  return (L.IntT, L.ConstantIntW n)
typecheckExpression (Boolean b) =
  return (L.BoolT, L.ConstantBoolW b)
typecheckExpression (Brack exp) =
  typecheckExpression exp
typecheckExpression (Id x) = do
  (tEnv, _, _) <- ask
  t <- E.loadE x (Err.TruthVarUndefined x) tEnv
  return (t, L.TruthHypothesisW t)
typecheckExpression (Abs x t b) = do
  (returnType, returnProof) <- local (updateTruthEnv $ E.save x t) (typecheckExpression b)
  return (L.ArrowT t returnType, L.AbstractionW t returnProof)
typecheckExpression (App x y) = do
  (xType, xProof) <- typecheckExpression x
  case xType of
    (L.ArrowT l r) -> do
      (yType, yProof) <- typecheckExpression y
      if yType == l
      then return (r, L.ApplicationW xProof yProof)
      else throwError (Err.InvalidArgType yType l)
    t ->
      throwError (Err.ExpectedArrow x xType)
typecheckExpression (AuditedVar trailRenames u) = do
  (_, wEnv, eEnv) <- ask
  validityVar <- E.loadE u (Err.ValidityVarUndefined u) wEnv
  case validityVar of
    (L.BoxT _ trailEnv _ t) ->
      let initialTrailVars = E.keys eEnv
          boxTrailVars = E.keys trailEnv
          (domain, codomain) = unzipTrailRenames trailRenames
      in
        if codomain == initialTrailVars then
          if domain == boxTrailVars then
            return (renameTypeTrailVars trailRenames t, L.ValidityHypothesisW u trailRenames)
          else
            throwError (Err.InvalidRenameDomain boxTrailVars domain)
        else
            throwError (Err.InvalidRenameCodomain initialTrailVars codomain)
    t -> throwError (Err.ValidityVarWrongType u validityVar)
typecheckExpression (AuditedUnit trailVar exp) = do
  let newTrailEnv = E.save trailVar (L.Reflexivity $ L.TruthHypothesisW L.IntT) E.empty
  (expType, expProof) <- local updateEnvs (typecheckExpression exp)
  return (L.BoxT trailVar newTrailEnv expProof expType, L.BoxIntroductionW newTrailEnv expProof)
  where
    updateEnvs (_, wEnv, eEnv) =
      (E.empty, wEnv, E.save trailVar (L.Reflexivity $ L.TruthHypothesisW L.IntT) E.empty)
typecheckExpression (AuditedComp u typ arg body) = do
  (argType, argProof) <- typecheckExpression arg
  case argType of
    (L.BoxT s trailEnv p t) -> do
      (bodyType, bodyProof) <- local (updateWitnessEnv $ E.save u argType) (typecheckExpression body)
      let subsitutedBodyType = subsituteTypeValidityVars ValidityVarSubParams{u=u, trailEnv=trailEnv, p=p} bodyType in
        return (subsitutedBodyType, L.BoxEliminationW s t bodyProof argProof)
    t -> throwError (Err.ExpectedBox argType)
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
    = do
  (_, _, eEnv) <- ask
  trail <- E.loadE trailVar (Err.TrailVarUndefined trailVar) eEnv
  (rType, rProof) <- local keepTruthEnv (typecheckExpression exp_r)
  (sType, sProof) <- local (updateForS rType) (typecheckExpression exp_s)
  (tType, tProof) <- local (updateForT rType) (typecheckExpression exp_t)
  (baType, baProof) <- local keepTruthEnv (typecheckExpression exp_ba)
  (bbType, bbProof) <- local keepTruthEnv (typecheckExpression exp_bb)
  (tiType, tiProof) <- local keepTruthEnv (typecheckExpression exp_ti)
  (absType, absProof) <- local (updateForAbs rType) (typecheckExpression exp_abs)
  (appType, appProof) <- local (updateForApp rType) (typecheckExpression exp_app)
  (letType, letProof) <- local (updateForLet rType) (typecheckExpression exp_let)
  (eType, eProof) <- local keepTruthEnv (typecheckExpression exp_e)
  if allEqual [rType, sType, tType, baType, bbType, tiType, absType, appType, letType, eType] then
    return (rType, L.TrailInspectionW trailVar rProof sProof tProof baProof bbProof tiProof absProof appProof letProof eProof)
  else throwError Err.InconsistentTrailMappings
  where
    keepTruthEnv (tEnv, _, _) = (tEnv, E.empty, E.empty)
    updateForS t = keepTruthEnv . updateTruthEnv (E.save s1 t)
    updateForT t = keepTruthEnv . updateTruthEnv (E.save t2 t . E.save t1 t)
    updateForAbs t = keepTruthEnv . updateTruthEnv (E.save abs1 t)
    updateForApp t = keepTruthEnv . updateTruthEnv (E.save app1 t . E.save app2 t)
    updateForLet t = keepTruthEnv . updateTruthEnv (E.save let1 t . E.save let2 t)
