module AudiComp.Interpreter where

import AudiComp.Core.Language as L
import Text.Printf
import AudiComp.Core.Util
import AudiComp.Core.Env as E
import AudiComp.Core.Errors.Interpreter as Err
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

interpretProgramEmptyEnvs :: Program -> Either String ValuePair
interpretProgramEmptyEnvs program =
  case interpretProgram program E.empty E.empty E.empty of
    (Right x) -> Right x
    (Left x) -> Left $ prettyShow x

interpretProgram :: Program -> Env ValuePair-> Env L.Witness -> Env L.Trail -> Either InterpreterE ValuePair
interpretProgram (Program exp) =
  interpretExpression exp

interpretExpression :: Exp -> Env ValuePair -> Env L.Witness -> Env L.Trail -> Either InterpreterE ValuePair
interpretExpression (Number n) _ _ _ =
  Right (IntV n, L.ConstantIntW n)
interpretExpression (Boolean b) _ _ _ =
  Right (BoolV b, L.ConstantBoolW b)
interpretExpression (Brack exp) tEnv wEnv eEnv =
  interpretExpression exp tEnv wEnv eEnv
interpretExpression (Id x) tEnv wEnv eEnv =
  E.loadE x (Err.TruthVarUndefined x) tEnv
interpretExpression (Abs x t b) tEnv wEnv eEnv = do
  witness <- computeWitness (Abs x t b) tEnv wEnv eEnv
  Right (ArrowV tEnv wEnv eEnv x b, L.AbstractionW t witness)
interpretExpression (App x y) tEnv wEnv eEnv =
  undefined
interpretExpression (AuditedVar trailRenames u) _ wEnv eEnv =
  undefined
interpretExpression (AuditedUnit trailVar exp) _ wEnv eEnv =
  undefined
interpretExpression (AuditedComp u typ arg body) tEnv wEnv eEnv =
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
    tEnv wEnv eEnv =
  undefined
