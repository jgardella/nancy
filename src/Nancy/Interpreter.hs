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

type InterpretEnv = Trail

type InterpretM = ReaderT InterpretEnv (ExceptT Err.InterpreterE (WriterT [String] Identity)) ValuePair

runInterpretM :: InterpretEnv -> InterpretM -> (Either Err.InterpreterE ValuePair, [String])
runInterpretM env m = runIdentity (runWriterT (runExceptT (runReaderT m env)))

interpretProgram :: Program -> (Either NancyError Value, [String])
interpretProgram (Program exp) =
  processResult
  $ runInterpretM unusedInitialTrail (interpretExpression bangWrappedExp)
  where
    unusedInitialTrail = L.RTrail $ L.IntWit 0
    bangWrappedExp =
      case exp of
        (Bang _ _) -> exp
        nonBangExp -> Bang nonBangExp (L.RTrail (getWit nonBangExp))
    processResult (result, logs) =
      (mapBoth InterpretErr fst result, logs)

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
      return (result, getReturnTrail lamTrail argTrail var varType resultTrail)
    _ ->
      throwError (Err.ExpectedLam lamVal)
  where
    updateTrailForArg lamTrail arg currentTrail =
      currentTrail <--> L.AppTrail lamTrail (L.RTrail $ getWit arg)
    updateTrailForBody lamTrail argTrail var varType currentTrail =
      currentTrail
      <--> L.AppTrail lamTrail argTrail
      <--> L.BaTrail var varType (getTarget lamTrail) (getTarget argTrail)
    getReturnTrail lamTrail argTrail var varType resultTrail =
      L.AppTrail lamTrail argTrail
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
      return (resultVal, getReturnTrail argTrail bangTrail bangVal body var varType resultTrail)
    _ -> throwError (Err.ExpectedBang argValue)
  where
    updateTrailForBody argTrail bangTrail bangVal body var varType currentTrail =
      currentTrail
      <--> L.LetTrail var varType argTrail (L.RTrail $ getWit body)
      <--> L.BbTrail var varType (getSource bangTrail) (getWit body)
      <--> trailSubOverAVar bangTrail var bangVal (getSource bangTrail) body
    getReturnTrail argTrail bangTrail bangVal body var varType resultTrail =
      L.LetTrail var varType argTrail (L.RTrail $ getWit body)
      <--> L.BbTrail var varType (getSource bangTrail) (getWit body)
      <--> trailSubOverAVar bangTrail var bangVal (getSource bangTrail) body
      <--> resultTrail
interpretExpression
  inspect@(Inspect
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
    foldForInspect =
      foldTrail TrailFoldFunctions{ rVal = return (rValue, rTrail),
                                    tFunc = foldFor2Trails tValue,
                                    baVal = return (baValue, baTrail),
                                    bbVal = return (bbValue, bbTrail),
                                    tiVal = return (tiValue, tiTrail),
                                    lamFunc = foldFor1Trail lamValue,
                                    appFunc = foldFor2Trails appValue,
                                    letFunc = foldFor2Trails letValue,
                                    trplFunc = foldForTrpl }
    foldFor1Trail value trail =
      case value of
        L.LamVal arg _ body -> do
          (value, foldTrail) <- foldForInspect trail
          (resultValue, resultTrail) <- interpretExpression (valueSubOverVar value arg body)
          return (resultValue, foldTrail <--> resultTrail)
        _ ->
          throwError Err.BadTrailValue
    foldFor2Trails value trail1 trail2 =
      case value of
        L.LamVal arg1 _ (L.Lam arg2 _ body) -> do
          (value1, foldTrail1) <- foldForInspect trail1
          (value2, foldTrail2) <- foldForInspect trail2
          (resultValue, resultTrail) <- interpretExpression (valueSubOverVar value1 arg1 (valueSubOverVar value2 arg2 body))
          return (resultValue, foldTrail1 <--> foldTrail2 <--> resultTrail)
        _ ->
          throwError Err.BadTrailValue
    foldForTrpl (TrailBranches rTrail tTrail baTrail bbTrail tiTrail lamTrail appTrail letTrail trplTrail) =
      case trplValue of
        L.LamVal rArg _
          (L.Lam tArg _
          (L.Lam baArg _
          (L.Lam bbArg _
          (L.Lam tiArg _
          (L.Lam lamArg _
          (L.Lam appArg _
          (L.Lam letArg _
          (L.Lam trplArg _ body)))))))) -> do
          (rVal, rFoldTrail) <- foldForInspect rTrail
          (tVal, tFoldTrail) <- foldForInspect tTrail
          (baVal, baFoldTrail) <- foldForInspect baTrail
          (bbVal, bbFoldTrail) <- foldForInspect bbTrail
          (tiVal, tiFoldTrail) <- foldForInspect tiTrail
          (lamVal, lamFoldTrail) <- foldForInspect lamTrail
          (appVal, appFoldTrail) <- foldForInspect appTrail
          (letVal, letFoldTrail) <- foldForInspect letTrail
          (trplVal, trplFoldTrail) <- foldForInspect trplTrail
          (resultValue, resultTrail) <-
            interpretExpression (valueSubOverVar rVal rArg
                                (valueSubOverVar tVal tArg
                                (valueSubOverVar baVal baArg
                                (valueSubOverVar bbVal bbArg
                                (valueSubOverVar tiVal tiArg
                                (valueSubOverVar lamVal lamArg
                                (valueSubOverVar appVal appArg
                                (valueSubOverVar letVal letArg
                                (valueSubOverVar trplVal trplArg body)))))))))
          return (resultValue, rFoldTrail <--> tFoldTrail <--> baFoldTrail <--> bbFoldTrail
                                 <--> tiFoldTrail <--> lamFoldTrail <--> appFoldTrail
                                 <--> letFoldTrail <--> trplFoldTrail <--> resultTrail)
        _ ->
          throwError Err.BadTrailValue
  (resultValue, resultTrail) <-
    local (updateTrailForFold rTrail tTrail baTrail bbTrail tiTrail lamTrail appTrail letTrail trplTrail)
      (foldForInspect currentTrail)
  return (resultValue, getReturnTrail rTrail tTrail baTrail bbTrail tiTrail lamTrail appTrail letTrail trplTrail resultTrail currentTrail)
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
    getReturnTrail rTrail tTrail baTrail bbTrail tiTrail lamTrail appTrail letTrail trplTrail resultTrail currentTrail =
      let
        finalTrplTrail = L.TrplTrail TrailBranches { rB = rTrail, tB = tTrail, baB = baTrail, bbB = bbTrail,
                                                      tiB = tiTrail, lamB = lamTrail, appB = appTrail,
                                                      letB = letTrail, trplB = trplTrail }
      in
        finalTrplTrail
        <--> L.TiTrail (currentTrail <--> finalTrplTrail) (fmap getWit branches)
        <--> resultTrail
