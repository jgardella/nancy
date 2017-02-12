module Typechecker where

import Parser
import qualified Types as T
import qualified Proof as P
import qualified Trail as E
import Env

type TypePair = T.Type, P.Proof

typecheckProgram :: Program -> Env TypePair -> Env T.Type -> Env E.Trail -> Maybe T.Type
typecheckProgram (Program exp) =
  typecheckExpression exp

typecheckExpression :: Exp -> Env TypePair -> Env P.Proof -> Env E.Trail -> Maybe T.Type
typecheckExpression (Number n) _ _ _ = Just $ T.Int, P.ConstantInt n
typecheckExpression (Boolean b) _ _ _ = Just $ T.Bool, P.ConstantBool b
typecheckExpression (Id x) tEnv pEnv eEnv =
  load x tEnv
typecheckExpression (Abs x t b) tEnv pEnv eEnv = do
  returnType, returnProof <- typecheckExpression b (save x t tEnv) pEnv eEnv
  Just $ (T.Arrow t returnType), P.Abstraction t returnProof
typecheckExpression (App x y) tEnv pEnv eEnv = do
  xType, xProof <- typecheckExpression x tEnv pEnv eEnv
  yType, yProof <- typecheckExpression y tEnv pEnv eEnv
  case xType of
    (T.Arrow l r) ->
      if yType == l then Just $ r, P.Application xProof yProof else Nothing
    _ -> Nothing
typecheckExpression (AuditedVar u sigma) _ pEnv eEnv = do
  case load u pEnv of
    (T.Audited t) ->
      Just $ renameTrailVars sigma t, P.ValidityHypothesis u sigma
    _ -> Nothing
typecheckExpression (AuditedUnit trailVar exp trail) _ pEnv eEnv = do
  --TODO (Eq(A, s t)?)
  expType, expProof <- typecheckExpression exp empty pEnv eEnv
  Just $ (T.Box trailVar expProof expType), (P.BoxIntroduction trailVar expProof)
typecheckExpression (AuditedComp u arg body) tEnv pEnv eEnv = do
  argType, argProof <- typecheckExpression arg tEnv pEnv eEnv
  case argType of
    (T.Box trailVar p t) ->
      bodyType, bodyProof <- typecheckExpression body tEnv (save u (T.Audited t) pEnv) eEnv
      subsitutedBodyType = subsituteValidtyVars u argProof bodyType
      Just $ subsitutedBodyType, (P.BoxElimination t bodyProof argProof)
    _ -> Nothing
typecheckExpression (TrailInspect trailVar trailReplacement) tEnv pEnv eEnv = do
  --TODO
  trail <- load trailVar eEnv
  replacementType, replacementProof <- typecheckExpression trailReplacement tEnv empty empty
  case replacementType of
    (T.Arrow T.Trail t) ->
      Just $ t, P.TrailInspection replacementProof
    _ -> Nothing
typecheckExpression (DerivedTerm exp trail) tEnv pEnv eEnv = do
  --TODO
  expType, expProof <- typecheckExpression exp tEnv pEnv eEnv
  trail <- computeTrail expType expProof t
  Just $ expType, t
