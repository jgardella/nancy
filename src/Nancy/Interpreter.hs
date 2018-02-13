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

type InterpretEnv = Trail

type InterpretM = ReaderT InterpretEnv (ExceptT Err.InterpreterE (WriterT [String] Identity)) ValuePair

runInterpretM :: InterpretEnv -> InterpretM -> (Either Err.InterpreterE ValuePair, [String])
runInterpretM env m = runIdentity (runWriterT (runExceptT (runReaderT m env)))

interpretProgram :: InterpretEnv -> Program -> (Either NancyError ValuePair, [String])
interpretProgram env (Program exp) =
  case runInterpretM env (interpretExpression exp) of
    (Right x, l) -> (Right x, l)
    (Left x, l) -> (Left $ InterpretErr x, l)

interpretExpression :: Exp -> InterpretM
interpretExpression (Number n) =
  return (IntVal n, L.RTrail $ L.IntWit n)
interpretExpression (Boolean b) =
  return (BoolVal b, L.RTrail $ L.BoolWit b)
interpretExpression (Brack exp) =
  interpretExpression exp
interpretExpression (Var x) =
  return (VarVal x, L.RTrail $ L.VarWit x)
interpretExpression exp@(AVar u) =
  return (AVarVal u, L.RTrail $ L.AVarWit u)
interpretExpression exp@(Lam arg argType body) =
  return (LamVal arg argType body, L.RTrail $ getWit exp)
interpretExpression (App lam arg) = do
  currentTrail <- ask
  (lamVal, lamTrail) <- interpretExpression lam
  case lamVal of
    (LamVal var varType body) -> do
      (argVal, argTrail) <- local (updateTrailForArg lamTrail arg) (interpretExpression arg)
      (result, resultTrail) <-
        local (updateTrailForBody lamTrail argTrail var varType)
          (interpretExpression (simpleTermSub argVal var body))
      return (result, getReturnTrail lamTrail argTrail var varType resultTrail currentTrail)
    _ ->
      throwError (Err.ExpectedLam lamVal)
  where
    updateTrailForArg lamTrail arg currentTrail =
      currentTrail <--> L.AppTrail lamTrail (L.RTrail $ getWit arg)
    updateTrailForBody lamTrail argTrail var varType currentTrail =
      currentTrail
      <--> L.AppTrail lamTrail argTrail
      <--> L.BaTrail var varType (getTarget lamTrail) (getTarget argTrail)
    getReturnTrail lamTrail argTrail var varType resultTrail currentTrail =
      currentTrail
      <--> L.AppTrail lamTrail argTrail
      <--> L.BaTrail var varType (getTarget lamTrail) (getTarget argTrail)
      <--> resultTrail
interpretExpression (Bang body bangTrail) = do
  currentTrail <- ask
  (bodyVal, bodyTrail) <- local (const bangTrail) (interpretExpression body)
  return (BangVal bodyVal (L.TTrail bangTrail bodyTrail), currentTrail)
interpretExpression (Let var varType arg body) = do
  currentTrail <- ask
  (argValue, argTrail) <- interpretExpression arg
  case argValue of
    (L.BangVal bangVal bangTrail) -> do
      (resultVal, resultTrail) <-
        local (updateTrailForBody argTrail bangTrail bangVal body var varType)
          (interpretExpression (auditedTermSub body bangTrail (getSource bangTrail) var))
      return (resultVal, getReturnTrail argTrail bangTrail bangVal body var varType resultTrail currentTrail)
    _ -> throwError (Err.ExpectedBang argValue)
  where
    updateTrailForBody argTrail bangTrail bangVal body var varType currentTrail =
      currentTrail
      <--> L.LetTrail var varType argTrail (L.RTrail $ getWit body)
      <--> L.BbTrail var varType (getSource bangTrail) (getWit body)
      <--> auditedTrailSub body bangTrail (getSource bangTrail) var
    getReturnTrail argTrail bangTrail bangVal body var varType resultTrail currentTrail =
      currentTrail
      <--> L.LetTrail var varType argTrail (L.RTrail $ getWit body)
      <--> L.BbTrail var varType (getSource bangTrail) (getWit body)
      <--> auditedTrailSub body bangTrail (getSource bangTrail) var
      <--> resultTrail
interpretExpression
  inspect@(Inspect
    (TrailBranches
      rExp
      tExp
      baExp
      bbExp
      tiExp
      lamExp
      appExp
      letExp
      trplExp
    )) =
  undefined
