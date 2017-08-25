module AudiComp.Interpreter where

import AudiComp.Core.Language as L
import Text.Printf
import AudiComp.Core.Util
import AudiComp.Core.Env as E
import AudiComp.Core.Errors.Interpreter as Err
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

type InterpretM = ReaderT InterpretEnv (ExceptT Err.InterpreterE Identity) ValuePair

runInterpretM :: InterpretEnv -> InterpretM -> Either Err.InterpreterE ValuePair
runInterpretM env m = runIdentity (runExceptT (runReaderT m env))

updateTruthEnv :: (Env L.ValuePair -> Env L.ValuePair) -> InterpretEnv -> InterpretEnv
updateTruthEnv f (tEnv, wEnv, eEnv) =
  (f tEnv, wEnv, eEnv)

updateWitnessEnv :: (Env L.Value -> Env L.Value) -> InterpretEnv -> InterpretEnv
updateWitnessEnv f (tEnv, wEnv, eEnv) =
  (tEnv, f wEnv, eEnv)

interpretProgramEmptyEnvs :: Program -> Either String ValuePair
interpretProgramEmptyEnvs program =
  case interpretProgram (E.empty, E.empty, E.empty) program of
    (Right x) -> Right x
    (Left x) -> Left $ prettyShow x

interpretProgram :: InterpretEnv -> Program -> Either InterpreterE ValuePair
interpretProgram env (Program exp) =
  runInterpretM env (interpretExpression exp)

interpretExpression :: Exp -> InterpretM
interpretExpression (Number n) =
  return (IntV n, L.ConstantIntW n)
interpretExpression (Boolean b) =
  return (BoolV b, L.ConstantBoolW b)
interpretExpression (Brack exp) =
  interpretExpression exp
interpretExpression (Id x) = do
  (tEnv, _, _) <- ask
  E.loadE x (Err.TruthVarUndefined x) tEnv
interpretExpression (Abs x t b) = do
  env <- ask
  witness <- computeWitness (Abs x t b)
  return (ArrowV env x b, L.AbstractionW t witness)
interpretExpression (App x y) = do
  (xVal, _) <- interpretExpression x
  witness <- computeWitness (App x y)
  case xVal of
    (ArrowV env var body) -> do
      yVal <- interpretExpression y
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
      return (L.BoxV s newTrailEnv witness value, witness)
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
  witness <- computeWitness (AuditedUnit trailVar exp)
  -- TODO: save current trail
  let newTrailEnv = E.save trailVar (L.Reflexivity $ L.TruthHypothesisW L.IntT) E.empty
  (expType, _) <- local updateEnvs (interpretExpression exp)
  return (L.BoxV trailVar newTrailEnv witness expType, L.BoxIntroductionW newTrailEnv witness)
  where
    updateEnvs (_, wEnv, eEnv) =
      (E.empty, wEnv, E.save trailVar (L.Reflexivity $ L.TruthHypothesisW L.IntT) E.empty)
interpretExpression (AuditedComp u typ arg body) = do
  witness <- computeWitness (AuditedComp u typ arg body)
  (argValue, _) <- interpretExpression arg
  case argValue of
    (L.BoxV s trailEnv w v) -> do
      (bodyValue, _) <- local (updateWitnessEnv $ E.save u argValue) (interpretExpression body)
      -- TODO: substitution
      return (bodyValue, witness)
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
  (tEnv, wEnv, eEnv) <- ask
  witness <- computeWitness inspect
  trail <- E.loadE trailVar (Err.TrailVarUndefined trailVar) eEnv
  trailFold trail
  where
    trailFold (Reflexivity _) = interpretExpression exp_r
    trailFold (Symmetry trl) = do
      sResult <- trailFold trl
      local (updateTruthEnv $ E.save s1 sResult) (interpretExpression exp_s)
    trailFold (Transitivity trl1 trl2) = do
      t1Result <- trailFold trl1
      t2Result <- trailFold trl2
      local (updateTruthEnv $ E.save t1 t1Result . E.save t2 t2Result) (interpretExpression exp_t)
    trailFold Beta{} = interpretExpression exp_ba
    trailFold BetaBox{} = interpretExpression exp_bb
    trailFold (AbsCompat _ trl) = do
      absResult <- trailFold trl
      local (updateTruthEnv $ E.save abs absResult) (interpretExpression exp_abs)
    trailFold (AppCompat trl1 trl2) = do
      app1Result <- trailFold trl1
      app2Result <- trailFold trl2
      local (updateTruthEnv $ E.save app1 app1Result . E.save app2 app2Result) (interpretExpression exp_app)
    trailFold (LetCompat _ _ trl1 trl2) = do
      let1Result <- trailFold trl1
      let2Result <- trailFold trl2
      local (updateTruthEnv $ E.save let1 let1Result . E.save let2 let2Result) (interpretExpression exp_let)
    trailFold (TrailInspectionT _
      trl1 trl2 trl3 trl4 trl5 trl6 trl7 trl8 trl9 trl10) = do
      e1Result <- trailFold trl1
      e2Result <- trailFold trl2
      e3Result <- trailFold trl3
      e4Result <- trailFold trl4
      e5Result <- trailFold trl5
      e6Result <- trailFold trl6
      e7Result <- trailFold trl7
      e8Result <- trailFold trl8
      e9Result <- trailFold trl9
      e10Result <- trailFold trl10
      local
        (updateTruthEnv
          $ E.save e1 e1Result
          . E.save e2 e2Result
          . E.save e3 e3Result
          . E.save e4 e4Result
          . E.save e5 e5Result
          . E.save e6 e6Result
          . E.save e7 e7Result
          . E.save e8 e8Result
          . E.save e9 e9Result
          . E.save e10 e10Result)
        (interpretExpression exp_e)
