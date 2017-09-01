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
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

type InterpretM = ReaderT InterpretEnv (ExceptT Err.InterpreterE Identity) ValuePair

runInterpretM :: InterpretEnv -> InterpretM -> Either Err.InterpreterE ValuePair
runInterpretM env m = runIdentity (runExceptT (runReaderT m env))

updateTruthEnv :: (Env L.Value -> Env L.Value) -> InterpretEnv -> InterpretEnv
updateTruthEnv f (tEnv, wEnv, eEnv) =
  (f tEnv, wEnv, eEnv)

updateWitnessEnv :: (Env L.Value -> Env L.Value) -> InterpretEnv -> InterpretEnv
updateWitnessEnv f (tEnv, wEnv, eEnv) =
  (tEnv, f wEnv, eEnv)

interpretProgram :: InterpretEnv -> Program -> Either NancyError ValuePair
interpretProgram env (Program exp) =
  case runInterpretM env (interpretExpression exp) of
    (Right x) -> Right x
    (Left x) -> Left $ InterpretErr x

interpretExpression :: Exp -> InterpretM
interpretExpression (Number n) =
  return (IntV n, L.ConstantIntW n)
interpretExpression (Boolean b) =
  return (BoolV b, L.ConstantBoolW b)
interpretExpression (Brack exp) =
  interpretExpression exp
interpretExpression (Id x) = do
  (tEnv, _, _) <- ask
  v <- E.loadE x (Err.TruthVarUndefined x) tEnv
  return (v, L.TruthHypothesisW x)
interpretExpression (Abs x t b) = do
  env <- ask
  witness <- computeWitness (Abs x t b)
  return (ArrowV env x b, witness)
interpretExpression (App x y) = do
  (xVal, _) <- interpretExpression x
  witness <- computeWitness (App x y)
  case xVal of
    (ArrowV env var body) -> do
      (yVal, yWitness) <- interpretExpression y
      (value, _) <- local (updateTruthEnv (E.save var yVal) . const env) (interpretExpression body)
      return (value, witness)
    _ ->
      throwError (Err.ExpectedArrow xVal)
interpretExpression (AuditedVar trailRenames u) = do
  (_, wEnv, eEnv) <- ask
  witness <- computeWitness (AuditedVar trailRenames u)
  validityVar <- E.loadE u (Err.ValidityVarUndefined u) wEnv
  case validityVar of
    (L.BoxV s trailEnv witness value) -> do
      newTrailEnv <- renameTrailVars trailEnv
      return (value, witness)
    _ -> throwError (Err.ExpectedBox validityVar)
  where
    renameTrailVars trailEnv =
      foldl (\result TrailRename{old=old, new=new} -> do
        newTrailEnv <- result
        value <- E.loadE old (Err.InvalidTrailRename old) trailEnv
        return $ E.save new value newTrailEnv)
      (return E.empty)
      trailRenames
interpretExpression (AuditedUnit trailVar exp) = do
  -- TODO: save current trail
  let newTrailEnv = E.save trailVar (L.Reflexivity $ L.TruthHypothesisW "") E.empty
  (expValue, expWitness) <- local updateEnvs (interpretExpression exp)
  return (L.BoxV trailVar newTrailEnv expWitness expValue, L.BoxIntroductionW newTrailEnv expWitness)
  where
    updateEnvs (_, wEnv, eEnv) =
      (E.empty, wEnv, E.save trailVar (L.Reflexivity $ L.TruthHypothesisW "") E.empty)
interpretExpression (AuditedComp u typ arg body) = do
  (argValue, argWitness) <- interpretExpression arg
  case argValue of
    (L.BoxV s trailEnv w v) -> do
      (bodyValue, bodyWitness) <- local (updateWitnessEnv $ E.save u argValue) (interpretExpression body)
      -- TODO: substitution
      return (bodyValue, L.BoxEliminationW u typ bodyWitness argWitness)
    t -> throwError (Err.ExpectedBox argValue)
interpretExpression
  inspect@(TrailInspect trailVar
    (L.ReflexivityM exp_r)
    (L.SymmetryM s1 exp_s)
    (L.TransitivityM t1 t2 exp_t)
    (L.BetaM exp_ba)
    (L.BetaBoxM exp_bb)
    (L.AbstractionM abs exp_abs)
    (L.ApplicationM app1 app2 exp_app)
    (L.LetM let1 let2 exp_let)
    (L.TrailInspectionM exp_ti)
    (L.ReplacementM e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 exp_e))
    = do
  witness <- computeWitness inspect
  (tEnv, wEnv, eEnv) <- ask
  trail <- E.loadE trailVar (Err.TrailVarUndefined trailVar) eEnv
  trailFold trail
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
    trailFold (TrailInspectionT _
      trl1 trl2 trl3 trl4 trl5 trl6 trl7 trl8 trl9 trl10) = do
      (e1Val, _) <- trailFold trl1
      (e2Val, _) <- trailFold trl2
      (e3Val, _) <- trailFold trl3
      (e4Val, _) <- trailFold trl4
      (e5Val, _) <- trailFold trl5
      (e6Val, _) <- trailFold trl6
      (e7Val, _) <- trailFold trl7
      (e8Val, _) <- trailFold trl8
      (e9Val, _) <- trailFold trl9
      (e10Val, _) <- trailFold trl10
      local
        (updateTruthEnv
          $ E.save e1 e1Val
          . E.save e2 e2Val
          . E.save e3 e3Val
          . E.save e4 e4Val
          . E.save e5 e5Val
          . E.save e6 e6Val
          . E.save e7 e7Val
          . E.save e8 e8Val
          . E.save e9 e9Val
          . E.save e10 e10Val)
        (interpretExpression exp_e)
