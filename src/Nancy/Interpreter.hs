module Nancy.Interpreter where

import Nancy.Core.Language as L
import Text.Printf
import Data.Either.Combinators
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

interpretProgram :: Program -> (Either NancyError ValuePair, [String])
interpretProgram (Program exp) =
  mapLeft InterpretErr
  $ runInterpretM unusedInitialTrail (interpretExpression bangWrappedExp)
  where
    unusedInitialTrail = L.RTrail $ L.IntWit 0
    bangWrappedExp =
      case exp of
        (Bang _ _) -> exp
        nonBangExp -> Bang nonBangExp (getWit nonBangExp)

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
          (interpretExpression (valueSubOverVar argVal var body))
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
          (interpretExpression (termSubOverAVar bangTrail var bangVal (getSource bangTrail) body))
      return (resultVal, getReturnTrail argTrail bangTrail bangVal body var varType resultTrail currentTrail)
    _ -> throwError (Err.ExpectedBang argValue)
  where
    updateTrailForBody argTrail bangTrail bangVal body var varType currentTrail =
      currentTrail
      <--> L.LetTrail var varType argTrail (L.RTrail $ getWit body)
      <--> L.BbTrail var varType (getSource bangTrail) (getWit body)
      <--> trailSubOverAVar bangTrail var bangVal (getSource bangTrail) body
    getReturnTrail argTrail bangTrail bangVal body var varType resultTrail currentTrail =
      currentTrail
      <--> L.LetTrail var varType argTrail (L.RTrail $ getWit body)
      <--> L.BbTrail var varType (getSource bangTrail) (getWit body)
      <--> trailSubOverAVar bangTrail var bangVal (getSource bangTrail) body
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
