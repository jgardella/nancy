module Nancy.Interpreter where

import Nancy.Core.Language as L
import Text.Printf
import Nancy.Core.Util
import Nancy.Core.Env as E
import Nancy.Core.Errors
import Nancy.Core.Errors.Interpreter as Err
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

type InterpretM = ReaderT InterpretEnv (ExceptT Err.InterpreterE (WriterT [String] (StateT Trail Identity))) ValuePair

runInterpretM :: InterpretEnv -> InterpretM -> ((Either Err.InterpreterE ValuePair, [String]), Trail)
runInterpretM env m = runIdentity (runStateT (runWriterT (runExceptT (runReaderT m env))) L.EmptyT)

updateTruthEnv :: (Env L.Value -> Env L.Value) -> InterpretEnv -> InterpretEnv
updateTruthEnv f (tEnv, wEnv, eEnv) =
  (f tEnv, wEnv, eEnv)

updateWitnessEnv :: (Env L.Value -> Env L.Value) -> InterpretEnv -> InterpretEnv
updateWitnessEnv f (tEnv, wEnv, eEnv) =
  (tEnv, f wEnv, eEnv)

updateTrailEnv :: (Env L.Trail -> Env L.Trail) -> InterpretEnv -> InterpretEnv
updateTrailEnv f (tEnv, wEnv, eEnv) =
  (tEnv, wEnv, f eEnv)

interpretProgram :: InterpretEnv -> Program -> (Either NancyError ValuePair, [String])
interpretProgram env (Program exp) =
  case runInterpretM env (interpretExpression exp) of
    ((Right x, l), _) -> (Right x, l)
    ((Left x, l), _) -> (Left $ InterpretErr x, l)

interpretExpression :: Exp -> InterpretM
interpretExpression (Number n) =
  return (IntV n, L.Reflexivity $ L.ConstantIntW n)
interpretExpression (Boolean b) =
  return (BoolV b, L.Reflexivity $ L.ConstantBoolW b)
interpretExpression (Brack exp) =
  interpretExpression exp
interpretExpression (Id x) = do
  (tEnv, _, _) <- ask
  v <- E.loadES x (Err.TruthVarUndefined x) tEnv
  return (v, L.Reflexivity $ L.TruthHypothesisW x)
interpretExpression (Abs x t b) = do
  env <- ask
  witness <- computeWitness (Abs x t b)
  return (ArrowV env x t b, L.Reflexivity witness)
interpretExpression (App x y) = do
  (xVal, xTrail) <- interpretExpression x
  case xVal of
    (ArrowV env var typ body) -> do
      (yVal, yTrail) <- interpretExpression y
      (value, valueTrail) <- local (updateTruthEnv (E.save var yVal) . const env) (interpretExpression body)
      xWitness <- local (updateTruthEnv (E.save var yVal) . const env) (computeWitness x)
      yWitness <- local (updateTruthEnv (E.save var yVal) . const env) (computeWitness y)
      let newTrail = L.Transitivity (L.AppCompat xTrail yTrail) (L.Beta typ xWitness yWitness)
      currentTrail <- get
      put $ L.Transitivity currentTrail newTrail
      return (value, newTrail)
    _ ->
      throwError (Err.ExpectedArrow xVal)
interpretExpression (AuditedVar trailRenames u) = do
  (_, wEnv, eEnv) <- ask
  witness <- computeWitness (AuditedVar trailRenames u)
  validityVar <- E.loadES u (Err.ValidityVarUndefined u) wEnv
  case validityVar of
    (L.BoxV s trailEnv witness value) -> do
      newTrailEnv <- renameTrailVars trailEnv
      return (value, L.Reflexivity witness)
    _ -> throwError (Err.ExpectedBox validityVar)
  where
    renameTrailVars trailEnv =
      foldl (\result TrailRename{old=old, new=new} -> do
        newTrailEnv <- result
        value <- E.loadES old (Err.InvalidTrailRename old) trailEnv
        return $ E.save new value newTrailEnv)
      (return E.empty)
      trailRenames
interpretExpression (AuditedUnit trailVar exp) = do
  currentTrail <- get
  let newTrailEnv = E.save trailVar currentTrail E.empty
  (expValue, expTrail) <- local (updateEnvs newTrailEnv) (interpretExpression exp)
  expWitness <- local (updateEnvs newTrailEnv) (computeWitness exp)
  return (L.BoxV trailVar newTrailEnv expWitness expValue, expTrail)
  where
    updateEnvs newTrailEnv (_, wEnv, eEnv) =
      (E.empty, wEnv, newTrailEnv)
interpretExpression (AuditedComp u typ arg body) = do
  (argValue, argTrail) <- interpretExpression arg
  case argValue of
    (L.BoxV s trailEnv w v) -> do
      (bodyValue, bodyTrail) <- local (updateWitnessEnv $ E.save u argValue) (interpretExpression body)
      argWitness <- local (updateTrailEnv $ const trailEnv) (computeWitness arg)
      bodyWitness <- local (updateTrailEnv $ const trailEnv) (computeWitness body)
      let newTrail =
            L.Transitivity
              (L.LetCompat u typ argTrail bodyTrail)
              (L.BetaBox u typ argWitness bodyWitness)
      put newTrail
      return (bodyValue, newTrail)
    t -> throwError (Err.ExpectedBox argValue)
interpretExpression
  inspect@(TrailInspect trailVar
    (L.ReflexivityM exp_r)
    (L.SymmetryM s1 exp_s)
    (L.TransitivityM t1 t2 exp_t)
    (L.BetaM exp_ba)
    (L.BetaBoxM exp_bb)
    (L.TrailInspectionM exp_ti)
    (L.AbstractionM abs exp_abs)
    (L.ApplicationM app1 app2 exp_app)
    (L.LetM let1 let2 exp_let))
    = do
  witness <- computeWitness inspect
  (tEnv, wEnv, eEnv) <- ask
  trail <- E.loadES trailVar (Err.TrailVarUndefined trailVar eEnv) eEnv
  (trailValue, trailTrail) <- trailFold trail
  currentTrail <- get
  put $ L.Transitivity currentTrail trailTrail
  return (trailValue, trailTrail)
  where
    trailFold (Reflexivity _) = interpretExpression exp_r
    trailFold (Symmetry trl) = do
      (sVal, _) <- trailFold trl
      local (updateTruthEnv $ E.save s1 sVal) (interpretExpression exp_s)
    trailFold (Transitivity trl1 trl2) = do
      (t1Val, _) <- trailFold trl1
      (t2Val, _) <- trailFold trl2
      local (updateTruthEnv $ E.save t1 t1Val . E.save t2 t2Val) (interpretExpression exp_t)
    trailFold Beta{} = interpretExpression exp_ba
    trailFold BetaBox{} = interpretExpression exp_bb
    trailFold (AbsCompat _ trl) = do
      (absVal, _) <- trailFold trl
      local (updateTruthEnv $ E.save abs absVal) (interpretExpression exp_abs)
    trailFold (AppCompat trl1 trl2) = do
      (app1Val, _) <- trailFold trl1
      (app2Val, _) <- trailFold trl2
      local (updateTruthEnv $ E.save app1 app1Val . E.save app2 app2Val) (interpretExpression exp_app)
    trailFold (LetCompat _ _ trl1 trl2) = do
      (let1Val, _) <- trailFold trl1
      (let2Val, _) <- trailFold trl2
      local (updateTruthEnv $ E.save let1 let1Val . E.save let2 let2Val) (interpretExpression exp_let)
    trailFold TrailInspectionT{} = interpretExpression exp_ti
