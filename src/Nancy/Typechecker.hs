module Nancy.Typechecker where

import Data.Function((&))
import Data.Either.Combinators
import Nancy.Parser
import Nancy.Core.Language as L
import Text.Printf
import Nancy.Core.Env as E
import Nancy.Core.Errors
import Nancy.Core.Errors.Typechecker as Err
import Nancy.Core.Util
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

type TypePair = (L.Type, L.Witness)

type TypecheckEnv = (Env L.Type, Env L.Type)

type TypecheckM = ReaderT TypecheckEnv (ExceptT Err.TypecheckerE Identity) TypePair

runTypecheckM :: TypecheckEnv -> TypecheckM -> Either Err.TypecheckerE TypePair
runTypecheckM env m = runIdentity (runExceptT (runReaderT m env))

updateTruthEnv :: (Env L.Type -> Env L.Type) -> TypecheckEnv -> TypecheckEnv
updateTruthEnv f (tEnv, wEnv) =
  (f tEnv, wEnv)

updateWitnessEnv :: (Env L.Type -> Env L.Type) -> TypecheckEnv -> TypecheckEnv
updateWitnessEnv f (tEnv, wEnv) =
  (tEnv, f wEnv)

typecheckProgram :: TypecheckEnv -> Program -> Either NancyError TypePair
typecheckProgram env (Program exp) =
  case runTypecheckM env (typecheckExpression exp) of
    (Right x) -> Right x
    (Left x) -> Left $ TypecheckErr x

typecheckExpression :: Exp -> TypecheckM
typecheckExpression (Number n) =
  return (L.IntType, L.IntWit n)
typecheckExpression (Boolean b) =
  return (L.BoolType, L.BoolWit b)
typecheckExpression (L.Brack exp) =
  typecheckExpression exp
typecheckExpression (L.Var x) = do
  (tEnv, _) <- ask
  varType <- E.loadE x (Err.TruthVarUndefined x) tEnv
  return (varType, L.VarWit x)
typecheckExpression (L.Lam arg argType body) = do
  (returnType, returnWit) <- local (updateTruthEnv $ E.save arg argType) (typecheckExpression body)
  return (L.ArrowType argType returnType, L.LamWit arg argType returnWit)
typecheckExpression (App left right) = do
  (leftType, leftWit) <- typecheckExpression left
  case leftType of
    (L.ArrowType argType returnType) -> do
      (rightType, rightWit) <- typecheckExpression right
      if rightType == argType
      then return (returnType, L.AppWit leftWit rightWit)
      else throwError (Err.InvalidArgType rightType argType)
    t ->
      throwError (Err.ExpectedArrow left leftType)
typecheckExpression (AVar u) = do
  (_, wEnv) <- ask
  varType <- E.loadE u (Err.ValidityVarUndefined u) wEnv
  return (varType, L.AVarWit u)
typecheckExpression (Bang body) = do
  (bodyType, bodyWit) <- local (updateTruthEnv $ const E.empty) (typecheckExpression body)
  return (L.BoxType bodyWit bodyType, L.BangWit bodyWit)
typecheckExpression (Let u uType arg body) = do
  (argType, argWit) <- typecheckExpression arg
  case argType of
    (L.BoxType boxWit boxType) | boxType == uType -> do
      (bodyType, bodyWit) <- local (updateWitnessEnv $ E.save u boxType) (typecheckExpression body)
      return (witSubOnType u boxWit bodyType, L.LetWit u uType bodyWit argWit)
    (L.BoxType _ boxType) | boxType /= uType ->
      throwError (Err.InvalidLetArgType uType boxType)
    t -> throwError (Err.ExpectedBox argType)
typecheckExpression
  (Inspect
    rExp
    tExp
    baExp
    bbExp
    tiExp
    lamExp
    appExp
    letExp
    trplExp)
    = do
  (rType, rWit) <- local keepWitEnv (typecheckExpression rExp)
  (tType, tWit) <- local keepWitEnv (typecheckExpression tExp)
  (baType, baWit) <- local keepWitEnv (typecheckExpression baExp)
  (bbType, bbWit) <- local keepWitEnv (typecheckExpression bbExp)
  (tiType, tiWit) <- local keepWitEnv (typecheckExpression tiExp)
  (lamType, lamWit) <- local keepWitEnv (typecheckExpression lamExp)
  (appType, appWit) <- local keepWitEnv (typecheckExpression appExp)
  (letType, letWit) <- local keepWitEnv (typecheckExpression letExp)
  (trplType, trplWit) <- local keepWitEnv (typecheckExpression trplExp)
  if allEqual [rType, baType, bbType, tiType]
    && lamType == createArrowType rType 1
    && tType == createArrowType rType 2
    && appType == createArrowType rType 2
    && letType == createArrowType rType 2
    && trplType == createArrowType rType 9
  then return (rType, L.TiWit rWit tWit baWit bbWit tiWit lamWit appWit letWit trplWit)
  else throwError Err.InconsistentTrailMappings
  where
    keepWitEnv (_, wEnv) = (E.empty, wEnv)
    createArrowType t n
      | n <= 1 = L.ArrowType t t
      | otherwise = L.ArrowType t (createArrowType t (n - 1))
