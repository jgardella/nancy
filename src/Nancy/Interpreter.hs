module Nancy.Interpreter where

import Nancy.Core.Language as L
import Data.Either.Combinators
import Nancy.Core.Util
import Nancy.Core.Errors.Interpreter as Err
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer

type InterpretEnv = Trail

type InterpretM = ReaderT InterpretEnv (ExceptT Err.InterpretError (WriterT [String] Identity)) ValuePair

runInterpretM :: InterpretEnv -> InterpretM -> (Either Err.InterpretError ValuePair, [String])
runInterpretM env m = runIdentity (runWriterT (runExceptT (runReaderT m env)))

interpretProgram :: Program -> (Either Err.InterpretError Value, [String])
interpretProgram (Program expr) =
  processResult
  $ runInterpretM unusedInitialTrail (interpretExpression expr)
  where
    unusedInitialTrail = L.RTrail $ L.IntWit 0
    processResult (result, logs) =
      (mapRight fst result, logs)

interpretExpression :: Exp -> InterpretM
interpretExpression (Number n) =
  return (IntVal n, L.RTrail $ L.IntWit n)
interpretExpression (Boolean b) =
  return (BoolVal b, L.RTrail $ L.BoolWit b)
interpretExpression (Brack expr) =
  interpretExpression expr
interpretExpression (Var x) =
  return (VarVal x, L.RTrail $ L.VarWit x)
interpretExpression (AVar u) =
  return (AVarVal u, L.RTrail $ L.AVarWit u)
interpretExpression expr@(Lam arg argType body) =
  return (LamVal arg argType body, L.RTrail $ getWit expr)
interpretExpression (App lam arg) = do
  (lamVal, lamTrail) <- interpretExpression lam
  case lamVal of
    (LamVal var varType body) -> do
      (argVal, argTrail) <- local (updateTrailForArg lamTrail arg) (interpretExpression arg)
      (result, resultTrail) <-
        local (updateTrailForBody lamTrail argTrail var varType)
          (interpretExpression (valueSubOverVar argVal var body))
      return (result, getReturnTrail lamTrail argTrail var varType resultTrail)
    _ ->
      throwError (Err.ExpectedLam lamVal)
  where
    updateTrailForArg lamTrail lamArg currentTrail =
      currentTrail <--> L.AppTrail lamTrail (L.RTrail $ getWit lamArg)
    updateTrailForBody lamTrail argTrail var varType currentTrail =
      currentTrail
      <--> L.AppTrail lamTrail argTrail
      <--> L.BaTrail var varType (getTarget lamTrail) (getTarget argTrail)
    getReturnTrail lamTrail argTrail var varType resultTrail =
      L.AppTrail lamTrail argTrail
      <--> L.BaTrail var varType (getTarget lamTrail) (getTarget argTrail)
      <--> resultTrail
interpretExpression expr@(Bang body bangTrail) = do
  (bodyVal, bodyTrail) <- local (const bangTrail) (interpretExpression body)
  return (BangVal bodyVal (L.TTrail bangTrail bodyTrail), L.RTrail $ getWit expr)
interpretExpression (Let letVar letVarType letArg letBody) = do
  (argValue, argTrail) <- interpretExpression letArg
  case argValue of
    (L.BangVal bangVal bangTrail) -> do
      (resultVal, resultTrail) <-
        local (updateTrailForBody argTrail bangTrail bangVal)
          (interpretExpression (termSubOverAVar bangTrail letVar bangVal (getSource bangTrail) letBody))
      return (resultVal, getReturnTrail argTrail bangTrail bangVal resultTrail)
    _ -> throwError (Err.ExpectedBang argValue)
  where
    updateTrailForBody argTrail bangTrail bangVal currentTrail =
      currentTrail
      <--> L.LetTrail letVar letVarType argTrail (L.RTrail $ getWit letBody)
      <--> L.BbTrail letVar letVarType (getSource bangTrail) (getWit letBody)
      <--> trailSubOverAVar bangTrail letVar bangVal (getSource bangTrail) letBody
    getReturnTrail argTrail bangTrail bangVal resultTrail =
      L.LetTrail letVar letVarType argTrail (L.RTrail $ getWit letBody)
      <--> L.BbTrail letVar letVarType (getSource bangTrail) (getWit letBody)
      <--> trailSubOverAVar bangTrail letVar bangVal (getSource bangTrail) letBody
      <--> resultTrail
interpretExpression
  (Inspect
    branches@(TrailBranches
      rExp
      tExp
      baExp
      bbExp
      tiExp
      lamExp
      appExp
      letExp
      trplExp
    )) = do
  currentTrail <- ask
  (rValue, rTrail) <- interpretExpression rExp
  (tValue, tTrail) <- local (updateTrailForT rTrail) (interpretExpression tExp)
  (baValue, baTrail) <- local (updateTrailForBa rTrail tTrail) (interpretExpression baExp)
  (bbValue, bbTrail) <- local (updateTrailForBb rTrail tTrail baTrail) (interpretExpression bbExp)
  (tiValue, tiTrail) <- local (updateTrailForTi rTrail tTrail baTrail bbTrail) (interpretExpression tiExp)
  (lamValue, lamTrail) <- local (updateTrailForLam rTrail tTrail baTrail bbTrail tiTrail) (interpretExpression lamExp)
  (appValue, appTrail) <- local (updateTrailForApp rTrail tTrail baTrail bbTrail tiTrail lamTrail) (interpretExpression appExp)
  (letValue, letTrail) <- local (updateTrailForLet rTrail tTrail baTrail bbTrail tiTrail lamTrail appTrail) (interpretExpression letExp)
  (trplValue, trplTrail) <- local (updateTrailForTrpl rTrail tTrail baTrail bbTrail tiTrail lamTrail appTrail letTrail) (interpretExpression trplExp)
  let
    foldBranchExps =
      valueToExp
      <$> TrailBranches {
        rB = rValue,
        tB = tValue,
        baB = baValue,
        bbB = bbValue,
        tiB = tiValue,
        lamB = lamValue,
        appB = appValue,
        letB = letValue,
        trplB = trplValue }
    foldExp =
      foldTrailToTerm foldBranchExps
        (updateTrailForFold rTrail tTrail baTrail bbTrail tiTrail lamTrail appTrail letTrail trplTrail currentTrail)
  (foldValue, foldTrail) <-
    local (updateTrailForFold rTrail tTrail baTrail bbTrail tiTrail lamTrail appTrail letTrail trplTrail)
          (interpretExpression foldExp)
  return (foldValue, getResultTrail rTrail tTrail baTrail bbTrail tiTrail lamTrail appTrail letTrail trplTrail foldTrail currentTrail)
  where
    base = fmap (L.RTrail . getWit) branches
    updateTrail trplTrail currentTrail =
      currentTrail
      <--> L.TrplTrail trplTrail
    updateTrailForT rTrail = updateTrail $ base { rB = rTrail }
    updateTrailForBa rTrail tTrail = updateTrail $ base { rB = rTrail, tB = tTrail }
    updateTrailForBb rTrail tTrail baTrail =
      updateTrail $ base { rB = rTrail, tB = tTrail, baB = baTrail }
    updateTrailForTi rTrail tTrail baTrail bbTrail =
      updateTrail $ base { rB = rTrail, tB = tTrail, baB = baTrail, bbB = bbTrail }
    updateTrailForLam rTrail tTrail baTrail bbTrail tiTrail =
      updateTrail $ base { rB = rTrail, tB = tTrail, baB = baTrail, bbB = bbTrail,
                           tiB = tiTrail }
    updateTrailForApp rTrail tTrail baTrail bbTrail tiTrail lamTrail =
      updateTrail $ base { rB = rTrail, tB = tTrail, baB = baTrail, bbB = bbTrail,
                           tiB = tiTrail, lamB = lamTrail }
    updateTrailForLet rTrail tTrail baTrail bbTrail tiTrail lamTrail appTrail =
      updateTrail $ base { rB = rTrail, tB = tTrail, baB = baTrail, bbB = bbTrail,
                           tiB = tiTrail, lamB = lamTrail, appB = appTrail }
    updateTrailForTrpl rTrail tTrail baTrail bbTrail tiTrail lamTrail appTrail letTrail =
      updateTrail $ base { rB = rTrail, tB = tTrail, baB = baTrail, bbB = bbTrail,
                           tiB = tiTrail, lamB = lamTrail, appB = appTrail, letB = letTrail }
    updateTrailForFold rTrail tTrail baTrail bbTrail tiTrail lamTrail appTrail letTrail trplTrail currentTrail =
      let
        finalTrplTrail = TrailBranches { rB = rTrail, tB = tTrail, baB = baTrail, bbB = bbTrail,
                                         tiB = tiTrail, lamB = lamTrail, appB = appTrail,
                                         letB = letTrail, trplB = trplTrail }
        currentWithTrpl = currentTrail <--> L.TrplTrail finalTrplTrail
      in
        currentTrail
        <--> L.TrplTrail finalTrplTrail
        <--> L.TiTrail currentWithTrpl (fmap getWit branches)
    getResultTrail rTrail tTrail baTrail bbTrail tiTrail lamTrail appTrail letTrail trplTrail foldTrail currentTrail =
      let
        finalTrplTrail = TrailBranches { rB = rTrail, tB = tTrail, baB = baTrail, bbB = bbTrail,
                                         tiB = tiTrail, lamB = lamTrail, appB = appTrail,
                                         letB = letTrail, trplB = trplTrail }
        currentWithTrpl = currentTrail <--> L.TrplTrail finalTrplTrail
      in
        currentTrail
        <--> L.TrplTrail finalTrplTrail
        <--> L.TiTrail currentWithTrpl (fmap getWit branches)
        <--> foldTrail
