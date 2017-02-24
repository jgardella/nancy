module Typechecker where

import Parser
import Types as T
import Env
import Util

type TypePair = (T.Type, T.Proof)

typecheckProgram :: Program -> Env T.Type -> Env T.Type -> Env T.Trail -> Maybe TypePair
typecheckProgram (Program exp) =
  typecheckExpression exp

typecheckExpression :: Exp -> Env T.Type -> Env T.Type -> Env T.Trail -> Maybe TypePair
typecheckExpression (Number n) _ _ _ = Just (T.Int, T.ConstantInt n)
typecheckExpression (Boolean b) _ _ _ = Just (T.Bool, T.ConstantBool b)
typecheckExpression (Id x) tEnv pEnv eEnv = do
  xType <- load x tEnv
  Just (xType, T.TruthHypothesis xType)
typecheckExpression (Abs x t b) tEnv pEnv eEnv = do
  (returnType, returnProof) <- typecheckExpression b (save x t tEnv) pEnv eEnv
  Just (T.Arrow t returnType, T.Abstraction t returnProof)
typecheckExpression (App x y) tEnv pEnv eEnv = do
  (xType, xProof) <- typecheckExpression x tEnv pEnv eEnv
  (yType, yProof) <- typecheckExpression y tEnv pEnv eEnv
  case xType of
    (T.Arrow l r) ->
      if yType == l then Just (r, T.Application xProof yProof) else Nothing
    _ -> Nothing
typecheckExpression (AuditedVar u oldTrailVar newTrailVar) _ pEnv eEnv = do
  validityVar <- load u pEnv
  case validityVar of
    (T.Audited t) ->
      Just (renameTypeTrailVars oldTrailVar newTrailVar t, T.ValidityHypothesis u newTrailVar)
    _ -> Nothing
typecheckExpression (AuditedUnit trailVar exp) _ pEnv eEnv = do
  (expType, expProof) <- typecheckExpression exp empty pEnv eEnv
  Just (T.Box eEnv expProof expType, T.BoxIntroduction eEnv expProof)
typecheckExpression (AuditedComp u arg body) tEnv pEnv eEnv = do
  (argType, argProof) <- typecheckExpression arg tEnv pEnv eEnv
  case argType of
    (T.Box trailVar p t) -> do
      (bodyType, bodyProof) <- typecheckExpression body tEnv (save u (T.Audited t) pEnv) eEnv
      let subsitutedBodyType = subsituteValidtyVars u argProof bodyType
      Just (subsitutedBodyType, T.BoxElimination t bodyProof argProof)
    _ -> Nothing
typecheckExpression
  (TrailInspect trailVar
    (Parser.Reflexivity r1 exp_r)
    (Parser.Symmetry s1 exp_s)
    (Parser.Transitivity t1 t2 exp_t)
    (Parser.Beta ba1 ba2 exp_ba)
    (Parser.BetaBox bb1 bb2 exp_bb)
    (Parser.TrailInspection ti1 ti2 exp_ti)
    (Parser.Abstraction abs1 exp_abs)
    (Parser.Application app1 app2 exp_app)
    (Parser.Let let1 let2 exp_let)
    (Parser.Replacement e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 exp_e))
    tEnv pEnv eEnv = do
  trail <- load trailVar eEnv
  (rType, rProof) <- typecheckExpression exp_r tEnv empty empty
  (sType, sProof) <- typecheckExpression exp_s tEnv empty empty
  (tType, tProof) <- typecheckExpression exp_t tEnv empty empty
  (baType, baProof) <- typecheckExpression exp_ba tEnv empty empty
  (bbType, bbProof) <- typecheckExpression exp_bb tEnv empty empty
  (tiType, tiProof) <- typecheckExpression exp_ti tEnv empty empty
  (absType, absProof) <- typecheckExpression exp_abs tEnv empty empty
  (appType, appProof) <- typecheckExpression exp_app tEnv empty empty
  (letType, letProof) <- typecheckExpression exp_let tEnv empty empty
  (eType, eProof) <- typecheckExpression exp_e tEnv empty empty
  if allEqual [rType, sType, tType, baType, bbType, tiType, absType, appType, letType, eType] then
    Just (rType, T.TrailInspectionP trailVar rProof sProof tProof baProof bbProof tiProof absProof appProof letProof eProof)
  else Nothing
