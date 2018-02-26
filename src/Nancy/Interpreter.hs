module Nancy.Interpreter where

import Nancy.Core.Language as L
import Nancy.Core.Util
import Nancy.Core.Output
import Nancy.Core.Errors.Interpreter as Err
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer

type InterpretEnv = Trail

type InterpretM = ReaderT InterpretEnv (ExceptT Err.InterpretError (WriterT [String] Identity)) (Value, Trail)

runInterpretM :: InterpretEnv -> InterpretM -> (Either Err.InterpretError (Value, Trail), [String])
runInterpretM env m = runIdentity (runWriterT (runExceptT (runReaderT m env)))

interpretProgram :: Program -> InterpreterOutput
interpretProgram (Program expr) =
  processResult
  $ runInterpretM unusedInitialTrail (interpretExpression expr)
  where
    unusedInitialTrail = L.RTrail $ L.IntWit 0
    processResult (result, logs) =
      case result of
        (Right (value, _)) -> InterpretSuccess (value, logs)
        (Left err) -> InterpretFailure (err, logs)

interpretExpression :: Expr -> InterpretM
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
interpretExpression (ALet letVar letVarType letArg letBody) = do
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
      <--> L.ALetTrail letVar letVarType argTrail (L.RTrail $ getWit letBody)
      <--> L.BbTrail letVar letVarType (getSource bangTrail) (getWit letBody)
      <--> trailSubOverAVar bangTrail letVar bangVal (getSource bangTrail) letBody
    getReturnTrail argTrail bangTrail bangVal resultTrail =
      L.ALetTrail letVar letVarType argTrail (L.RTrail $ getWit letBody)
      <--> L.BbTrail letVar letVarType (getSource bangTrail) (getWit letBody)
      <--> trailSubOverAVar bangTrail letVar bangVal (getSource bangTrail) letBody
      <--> resultTrail
interpretExpression (Inspect branches) = do
  currentTrail <- ask
  (branchValues, trplTrail) <- interpretBranches
  let foldExpr = foldTrailToTerm (valueToExpr <$> branchValues) (addTrplTrail trplTrail currentTrail)
  (foldValue, foldTrail) <- local (addTrplTrail trplTrail) (interpretExpression foldExpr)
  return (foldValue, getResultTrail trplTrail foldTrail currentTrail)
  where
    baseTrailList = trailBranchesToList (L.RTrail . getWit <$> branches)
    addTrplTrail trplTrail currentTrail =
      currentTrail <--> L.TrplTrail trplTrail
    interpretBranchesFold acc (idx, branch) = do
      (values, trails) <- acc
      let trailBranchList = trails ++ snd (splitAt idx baseTrailList)
          maybeTrplTrail = trailBranchesFromList trailBranchList
      case maybeTrplTrail of
        (Just trplTrail) -> do
          (branchValue, branchTrail) <- local (addTrplTrail trplTrail) (interpretExpression branch)
          return (values ++ [branchValue], trails ++ [branchTrail])
        Nothing ->
          throwError Err.InvalidTrailBranchList
    interpretBranches = do
      (resultValues, resultTrail) <-
        foldl interpretBranchesFold
          (return ([], []))
          (zip [0..] (trailBranchesToList branches))
      case (trailBranchesFromList resultValues, trailBranchesFromList resultTrail) of
        (Just branchValues, Just branchTrail) ->
          return (branchValues, branchTrail)
        _ ->
          throwError Err.InvalidTrailBranchList
    getResultTrail trplTrail foldTrail currentTrail =
      let
        currentWithTrpl = currentTrail <--> L.TrplTrail trplTrail
      in
        currentTrail
        <--> L.TrplTrail trplTrail
        <--> L.TiTrail currentWithTrpl (fmap getWit branches)
        <--> foldTrail
