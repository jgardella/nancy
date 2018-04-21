module Nancy.Typechecker where

import Nancy.Core.Language as L
import Nancy.Core.Env as E
import Nancy.Core.Output
import Nancy.Core.Errors.Typechecker as Err
import Nancy.Core.Util
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader

type TypecheckEnv = (Env L.Type, Env L.Type)

type TypecheckM = ReaderT TypecheckEnv (ExceptT Err.TypecheckError Identity) (Type, Witness)

runTypecheckM :: TypecheckEnv -> TypecheckM -> Either Err.TypecheckError (Type, Witness)
runTypecheckM env m = runIdentity (runExceptT (runReaderT m env))

updateTruthEnv :: (Env L.Type -> Env L.Type) -> TypecheckEnv -> TypecheckEnv
updateTruthEnv f (tEnv, wEnv) =
  (f tEnv, wEnv)

updateWitnessEnv :: (Env L.Type -> Env L.Type) -> TypecheckEnv -> TypecheckEnv
updateWitnessEnv f (tEnv, wEnv) =
  (tEnv, f wEnv)

typecheckProgram :: TypecheckEnv -> Program -> TypecheckerOutput
typecheckProgram env (Program term) =
  case runTypecheckM env (typecheckTerm term) of
    (Right typePair) -> TypecheckSuccess typePair
    (Left err) -> TypecheckFailure err

typecheckTerm :: Term -> TypecheckM
typecheckTerm (Number n) =
  return (L.IntType, L.Number n)
typecheckTerm (Boolean b) =
  return (L.BoolType, L.Boolean b)
typecheckTerm (L.Brack term) =
  typecheckTerm term
typecheckTerm (L.Var x) = do
  (tEnv, _) <- ask
  varType <- E.loadE x (Err.TruthVarUndefined x) tEnv
  return (varType, L.Var x)
typecheckTerm (L.Lam arg argType body) = do
  (returnType, returnWit) <- local (updateTruthEnv $ E.save arg argType) (typecheckTerm body)
  return (L.LamType argType returnType, L.Lam arg argType returnWit)
typecheckTerm (App left right) = do
  (leftType, leftWit) <- typecheckTerm left
  case leftType of
    (L.LamType argType returnType) -> do
      (rightType, rightWit) <- typecheckTerm right
      if rightType == argType
      then return (returnType, L.App leftWit rightWit)
      else throwError (Err.InvalidArgType rightType argType)
    _ ->
      throwError (Err.ExpectedLam left leftType)
typecheckTerm plusTerm@(Plus leftTerm rightTerm) = do
  (leftType, leftWit) <- typecheckTerm leftTerm
  (rightType, rightWit) <- typecheckTerm rightTerm
  case (leftType, rightType) of
    (L.IntType, L.IntType) ->
      return (L.IntType, L.Plus leftWit rightWit)
    _ ->
      throwError (Err.InvalidPlusArgs plusTerm)
typecheckTerm eqTerm@(Eq leftTerm rightTerm) = do
  (leftType, leftWit) <- typecheckTerm leftTerm
  (rightType, rightWit) <- typecheckTerm rightTerm
  case (leftType, rightType) of
    (L.IntType, L.IntType) ->
      return (L.BoolType, L.Eq leftWit rightWit)
    (L.BoolType, L.BoolType) ->
      return (L.BoolType, L.Eq leftWit rightWit)
    _ ->
      throwError (Err.InvalidEqArgs eqTerm)
typecheckTerm (Ite condTerm thenTerm elseTerm) = do
  (condType, condWit) <- typecheckTerm condTerm
  case condType of
    L.BoolType -> do
      (thenType, thenWit) <- typecheckTerm thenTerm
      (elseType, elseWit) <- typecheckTerm elseTerm
      if thenType == elseType then
        return (thenType, L.Ite condWit thenWit elseWit)
      else
        throwError (Err.InvalidIfBranches thenTerm thenType elseTerm elseType)
    _ ->
      throwError (Err.InvalidIfCond condTerm condType)
typecheckTerm (AVar u) = do
  (_, wEnv) <- ask
  varType <- E.loadE u (Err.ValidityVarUndefined u) wEnv
  return (varType, L.AVar u)
typecheckTerm (Bang body _) = do
  (bodyType, bodyWit) <- local (updateTruthEnv $ const E.empty) (typecheckTerm body)
  return (L.BangType bodyType bodyWit, L.Bang bodyWit L.NoTrail)
typecheckTerm (ALet u uType arg body) = do
  (argType, argWit) <- typecheckTerm arg
  case argType of
    (L.BangType bangType bangWit) | bangType == uType -> do
      (bodyType, bodyWit) <- local (updateWitnessEnv $ E.save u bangType) (typecheckTerm body)
      return (witSubOverAVarOnType u bangWit bodyType, L.ALet u uType bodyWit argWit)
    (L.BangType bangType _) | bangType /= uType ->
      throwError (Err.InvalidLetArgType uType bangType)
    _ -> throwError (Err.ExpectedBang argType)
typecheckTerm inspectTerm@(Inspect termBranches) = do
  (branchTypes, branchWits) <- typecheckBranches termBranches
  if branchTypes == createExpectedBranches (r branchTypes) then
    return (r branchTypes, L.Inspect branchWits)
  else
    throwError (Err.BadInspectBranch inspectTerm)
  where
    keepWitEnv (_, wEnv) = (E.empty, wEnv)
    typecheckBranches branches = do
      mapResult <- mapTrailBranchesM (local keepWitEnv . typecheckTerm) branches
      return $ unzipTrailBranches mapResult
    createLamType :: L.Type -> Integer -> L.Type
    createLamType t n
      | n < 1 = t
      | n == 1 = L.LamType t t
      | otherwise = L.LamType t (createLamType t (n - 1))
    createExpectedBranches baseType = createLamType baseType <$> L.trailBranchArity
