module Nancy.Typechecker where

import Data.Function((&))
import Data.Either.Combinators
import Nancy.Parser
import Nancy.Core.Language as L
import Text.Printf
import Nancy.Core.Env as E
import Nancy.Core.Errors
import Nancy.Core.Errors.Typechecker as Err
import Nancy.Core.Util
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

type TypePair = (L.Type, L.Witness)

type TypecheckEnv = (Env L.Type, Env L.Type)

type TypecheckM = ReaderT TypecheckEnv (ExceptT Err.TypecheckerE Identity) TypePair

runTypecheckM :: TypecheckEnv -> TypecheckM -> Either Err.TypecheckerE TypePair
runTypecheckM env m = runIdentity (runExceptT (runReaderT m env))

updateTruthEnv :: (Env L.Type -> Env L.Type) -> TypecheckEnv -> TypecheckEnv
updateTruthEnv f (tEnv, wEnv) =
  (f tEnv, wEnv)

updateWitnessEnv :: (Env L.Type -> Env L.Type) -> TypecheckEnv -> TypecheckEnv
updateWitnessEnv f (tEnv, wEnv) =
  (tEnv, f wEnv)

typecheckProgram :: TypecheckEnv -> Program -> Either NancyError TypePair
typecheckProgram env (Program exp) =
  case runTypecheckM env (typecheckExpression exp) of
    (Right x) -> Right x
    (Left x) -> Left $ TypecheckErr x

typecheckExpression :: Exp -> TypecheckM
typecheckExpression (Number n) =
  return (L.IntT, L.IntWit n)
typecheckExpression (Boolean b) =
  return (L.BoolT, L.BoolWit b)
typecheckExpression (Brack exp) =
  typecheckExpression exp
typecheckExpression (Id x) = do
  (tEnv, _) <- ask
  varType <- E.loadE x (Err.TruthVarUndefined x) tEnv
  return (varType, L.VarWit x)
typecheckExpression (Abs arg argType body) = do
  (returnType, returnProof) <- local (updateTruthEnv $ E.save arg argType) (typecheckExpression body)
  return (L.ArrowT argType returnType, L.AbsWit arg argType returnProof)
typecheckExpression (App left right) = do
  (leftType, leftProof) <- typecheckExpression left
  case leftType of
    (L.ArrowT argType returnType) -> do
      (rightType, rightProof) <- typecheckExpression right
      if rightType == argType
      then return (returnType, L.AppWit leftProof rightProof)
      else throwError (Err.InvalidArgType rightType argType)
    t ->
      throwError (Err.ExpectedArrow left leftType)
typecheckExpression (AuditedVar u) = do
  (_, wEnv) <- ask
  varType <- E.loadE u (Err.ValidityVarUndefined u) wEnv
  return (varType, L.AVarWit u)
typecheckExpression (AuditedUnit body) = do
  (bodyType, bodyProof) <- local (updateTruthEnv $ konst E.empty) (typecheckExpression body)
  return (L.BoxT bodyProof bodyType, L.BangWit bodyProof)
typecheckExpression (AuditedComp u uType arg body) = do
  (argType, argProof) <- typecheckExpression arg
  case argType of
    (L.BoxT boxProof boxType) | boxType == uType -> do
      (bodyType, bodyProof) <- local (updateWitnessEnv $ E.save u boxType) (typecheckExpression body)
      let subsitutedBodyType = subsituteTypeValidityVars ValidityVarSubParams{u=u, trailEnv=trailEnv, p=p} bodyType in
        return (subsitutedBodyType, L.BoxEliminationW s t bodyProof argProof)
    (L.BoxT _ boxType) | boxType <> uType ->
      throwError (Err.InvalidLetArgType uType boxType)
    t -> throwError (Err.ExpectedBox argType)
typecheckExpression
  (TrailInspect trailVar
    (L.ReflexivityM exp_r)
    (L.TransitivityM t1 t2 exp_t)
    (L.BetaM exp_ba)
    (L.BetaBoxM exp_bb)
    (L.TrailInspectionM exp_ti)
    (L.AbstractionM abs1 exp_abs)
    (L.ApplicationM app1 app2 exp_app)
    (L.LetM let1 let2 exp_let))
    = do
  (_, _, eEnv) <- ask
  trail <- E.loadE trailVar (Err.TrailVarUndefined trailVar) eEnv
  (rType, rProof) <- local keepTruthEnv (typecheckExpression exp_r)
  (tType, tProof) <- local (updateForT rType) (typecheckExpression exp_t)
  (baType, baProof) <- local keepTruthEnv (typecheckExpression exp_ba)
  (bbType, bbProof) <- local keepTruthEnv (typecheckExpression exp_bb)
  (tiType, tiProof) <- local keepTruthEnv (typecheckExpression exp_ti)
  (absType, absProof) <- local (updateForAbs rType) (typecheckExpression exp_abs)
  (appType, appProof) <- local (updateForApp rType) (typecheckExpression exp_app)
  (letType, letProof) <- local (updateForLet rType) (typecheckExpression exp_let)
  if allEqual [rType, tType, baType, bbType, tiType, absType, appType, letType] then
    return (rType, L.TrailInspectionW rProof tProof baProof bbProof tiProof absProof appProof letProof)
  else throwError Err.InconsistentTrailMappings
  where
    keepTruthEnv (tEnv, _, _) = (tEnv, E.empty, E.empty)
    updateForT t = keepTruthEnv . updateTruthEnv (E.save t2 t . E.save t1 t)
    updateForAbs t = keepTruthEnv . updateTruthEnv (E.save abs1 t)
    updateForApp t = keepTruthEnv . updateTruthEnv (E.save app1 t . E.save app2 t)
    updateForLet t = keepTruthEnv . updateTruthEnv (E.save let1 t . E.save let2 t)
