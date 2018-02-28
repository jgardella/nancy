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
typecheckProgram env (Program expr) =
  case runTypecheckM env (typecheckExpression expr) of
    (Right typePair) -> TypecheckSuccess typePair
    (Left err) -> TypecheckFailure err

typecheckExpression :: Expr -> TypecheckM
typecheckExpression (Number n) =
  return (L.IntType, L.IntWit n)
typecheckExpression (Boolean b) =
  return (L.BoolType, L.BoolWit b)
typecheckExpression (L.Brack expr) =
  typecheckExpression expr
typecheckExpression (L.Var x) = do
  (tEnv, _) <- ask
  varType <- E.loadE x (Err.TruthVarUndefined x) tEnv
  return (varType, L.VarWit x)
typecheckExpression (L.Lam arg argType body) = do
  (returnType, returnWit) <- local (updateTruthEnv $ E.save arg argType) (typecheckExpression body)
  return (L.LamType argType returnType, L.LamWit arg argType returnWit)
typecheckExpression (App left right) = do
  (leftType, leftWit) <- typecheckExpression left
  case leftType of
    (L.LamType argType returnType) -> do
      (rightType, rightWit) <- typecheckExpression right
      if rightType == argType
      then return (returnType, L.AppWit leftWit rightWit)
      else throwError (Err.InvalidArgType rightType argType)
    _ ->
      throwError (Err.ExpectedLam left leftType)
typecheckExpression plusExpr@(Plus leftExpr rightExpr) = do
  (leftType, leftWit) <- typecheckExpression leftExpr
  (rightType, rightWit) <- typecheckExpression rightExpr
  case (leftType, rightType) of
    (L.IntType, L.IntType) ->
      return (L.IntType, L.PlusWit leftWit rightWit)
    _ ->
      throwError (Err.InvalidPlusArgs plusExpr)
typecheckExpression eqExpr@(Eq leftExpr rightExpr) = do
  (leftType, leftWit) <- typecheckExpression leftExpr
  (rightType, rightWit) <- typecheckExpression rightExpr
  case (leftType, rightType) of
    (L.IntType, L.IntType) ->
      return (L.BoolType, L.EqWit leftWit rightWit)
    (L.BoolType, L.BoolType) ->
      return (L.BoolType, L.EqWit leftWit rightWit)
    _ ->
      throwError (Err.InvalidEqArgs eqExpr)
typecheckExpression (Ite condExpr thenExpr elseExpr) = do
  (condType, condWit) <- typecheckExpression condExpr
  case condType of
    L.BoolType -> do
      (thenType, thenWit) <- typecheckExpression thenExpr
      (elseType, elseWit) <- typecheckExpression elseExpr
      if thenType == elseType then
        return (thenType, L.IteWit condWit thenWit elseWit)
      else
        throwError (Err.InvalidIfBranches thenExpr thenType elseExpr elseType)
    _ ->
      throwError (Err.InvalidIfCond condExpr condType)
typecheckExpression (AVar u) = do
  (_, wEnv) <- ask
  varType <- E.loadE u (Err.ValidityVarUndefined u) wEnv
  return (varType, L.AVarWit u)
typecheckExpression (Bang body _) = do
  (bodyType, bodyWit) <- local (updateTruthEnv $ const E.empty) (typecheckExpression body)
  return (L.BangType bodyType bodyWit, L.BangWit bodyWit)
typecheckExpression (ALet u uType arg body) = do
  (argType, argWit) <- typecheckExpression arg
  case argType of
    (L.BangType bangType bangWit) | bangType == uType -> do
      (bodyType, bodyWit) <- local (updateWitnessEnv $ E.save u bangType) (typecheckExpression body)
      return (witSubOverAVarOnType u bangWit bodyType, L.ALetWit u uType bodyWit argWit)
    (L.BangType bangType _) | bangType /= uType ->
      throwError (Err.InvalidLetArgType uType bangType)
    _ -> throwError (Err.ExpectedBang argType)
typecheckExpression inspectExpr@(Inspect exprBranches) = do
  (branchTypes, branchWits) <- typecheckBranches exprBranches
  if branchTypes == createExpectedBranches (r branchTypes) then
    return (r branchTypes, L.TiWit branchWits)
  else
    throwError (Err.BadInspectBranch inspectExpr)
  where
    keepWitEnv (_, wEnv) = (E.empty, wEnv)
    typecheckBranches branches = do
      mapResult <- mapTrailBranchesM (local keepWitEnv . typecheckExpression) branches
      return $ unzipTrailBranches mapResult
    createLamType :: L.Type -> Integer -> L.Type
    createLamType t n
      | n < 1 = t
      | n == 1 = L.LamType t t
      | otherwise = L.LamType t (createLamType t (n - 1))
    createExpectedBranches baseType = createLamType baseType <$> L.trailBranchArity
