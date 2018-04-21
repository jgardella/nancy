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
interpretProgram (Program term) =
  processResult
  $ runInterpretM unusedInitialTrail (interpretTerm term)
  where
    unusedInitialTrail = L.RTrail $ L.Number 0
    processResult (result, logs) =
      case result of
        (Right (value, _)) -> InterpretSuccess (value, logs)
        (Left err) -> InterpretFailure (err, logs)

interpretTerm :: Term -> InterpretM
interpretTerm (Number n) =
  return (IntVal n, L.RTrail $ L.Number n)
interpretTerm (Boolean b) =
  return (BoolVal b, L.RTrail $ L.Boolean b)
interpretTerm (Brack term) =
  interpretTerm term
interpretTerm (Var x) =
  return (VarVal x, L.RTrail $ L.Var x)
interpretTerm (AVar u) =
  return (AVarVal u, L.RTrail $ L.AVar u)
interpretTerm term@(Lam arg argType body) =
  return (LamVal arg argType body, L.RTrail $ getWit term)
interpretTerm (App lam arg) = do
  (lamVal, lamTrail) <- interpretTerm lam
  case lamVal of
    (LamVal var varType body) -> do
      (argVal, argTrail) <- local (updateTrailForArg lamTrail arg) (interpretTerm arg)
      (result, resultTrail) <-
        local (updateTrailForBody lamTrail argTrail var varType)
          (interpretTerm (valueSubOverVar argVal var body))
      return (result, getReturnTrail lamTrail argTrail var varType resultTrail)
    _ ->
      throwError (Err.ExpectedLam lamVal)
  where
    updateTrailForArg lamTrail lamArg currentTrail =
      currentTrail <--> L.AppTrail lamTrail (L.RTrail $ getWit lamArg)
    updateTrailForBody lamTrail argTrail var varType currentTrail =
      currentTrail
      <--> L.AppTrail lamTrail argTrail
      <--> L.BaTrail var varType (getTarget argTrail) (getTarget lamTrail)
    getReturnTrail lamTrail argTrail var varType resultTrail =
      L.AppTrail lamTrail argTrail
      <--> L.BaTrail var varType (getTarget argTrail) (getTarget lamTrail)
      <--> resultTrail
interpretTerm plusTerm@(Plus leftTerm rightTerm) = do
  (leftVal, leftTrail) <- interpretTerm leftTerm
  (rightVal, rightTrail) <- local (updateTrailForRight leftTrail) (interpretTerm rightTerm)
  case (leftVal, rightVal) of
    (IntVal leftInt, IntVal rightInt) ->
      return (IntVal (leftInt + rightInt), L.PlusTrail leftTrail rightTrail)
    _ ->
      throwError (Err.InvalidPlusArgs plusTerm)
  where
    updateTrailForRight leftTrail currentTrail =
      currentTrail <--> L.PlusTrail leftTrail (L.RTrail (getWit rightTerm))
interpretTerm eqTerm@(Eq leftTerm rightTerm) = do
  (leftVal, leftTrail) <- interpretTerm leftTerm
  (rightVal, rightTrail) <- local (updateTrailForRight leftTrail) (interpretTerm rightTerm)
  case (leftVal, rightVal) of
    (IntVal leftInt, IntVal rightInt) ->
      return (BoolVal (leftInt == rightInt), L.EqTrail leftTrail rightTrail)
    (BoolVal leftBool, BoolVal rightBool) ->
      return (BoolVal (leftBool == rightBool), L.EqTrail leftTrail rightTrail)
    _ ->
      throwError (Err.InvalidEqArgs eqTerm)
  where
    updateTrailForRight leftTrail currentTrail =
      currentTrail <--> L.EqTrail leftTrail (L.RTrail (getWit rightTerm))
interpretTerm (Ite condTerm thenTerm elseTerm) = do
  (condVal, condTrail) <- interpretTerm condTerm
  (thenVal, thenTrail) <- local (updateTrailForThen condTrail) (interpretTerm thenTerm)
  (elseVal, elseTrail) <- local (updateTrailForElse condTrail thenTrail) (interpretTerm elseTerm)
  case condVal of
    (L.BoolVal bool) | bool ->
      return (thenVal, L.IteTrail condTrail thenTrail elseTrail)
    (L.BoolVal _) ->
      return (elseVal, L.IteTrail condTrail thenTrail elseTrail)
    _ ->
      throwError (Err.InvalidIfCond condTerm condVal)
  where
    updateTrailForThen condTrail currentTrail =
      currentTrail <--> L.IteTrail condTrail (L.RTrail (getWit thenTerm)) (L.RTrail (getWit elseTerm))
    updateTrailForElse condTrail thenTrail currentTrail =
      currentTrail <--> L.IteTrail condTrail thenTrail (L.RTrail (getWit elseTerm))
interpretTerm term@(Bang body bangTrail) = do
  (bodyVal, bodyTrail) <- local (const bangTrail) (interpretTerm body)
  return (BangVal bodyVal (TrailWithMode (Standard, L.TTrail bangTrail bodyTrail)), L.RTrail $ getWit term)
interpretTerm (ALet letVar letVarType letArg letBody) = do
  (argValue, argTrail) <- interpretTerm letArg
  case argValue of
    (L.BangVal bangVal (TrailWithMode (_, bangTrail))) -> do
      (resultVal, resultTrail) <-
        local (updateTrailForBody argTrail bangTrail bangVal)
          (interpretTerm (termSubOverAVar bangTrail letVar bangVal (getSource bangTrail) letBody))
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
interpretTerm (Inspect branches) = do
  currentTrail <- ask
  (branchValues, trplTrail) <- interpretBranches
  let foldTerm = foldTrailToExpr (valueToTerm <$> branchValues) (addTrplTrail trplTrail currentTrail)
  (foldValue, foldTrail) <- local (addTrplTrail trplTrail) (interpretTerm foldTerm)
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
          (branchValue, branchTrail) <- local (addTrplTrail trplTrail) (interpretTerm branch)
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
