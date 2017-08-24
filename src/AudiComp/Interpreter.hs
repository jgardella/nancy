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
interpretExpression (App x y) =
  undefined
interpretExpression (AuditedVar trailRenames u) =
  undefined
interpretExpression (AuditedUnit trailVar exp) =
  undefined
interpretExpression (AuditedComp u typ arg body) =
  undefined
interpretExpression
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
    =
  undefined
