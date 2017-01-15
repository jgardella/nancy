module Typechecker where

import Parser
import qualified Types as T
import qualified Proof as P
import Env

type TCValue = (T.Type, P.Proof)

typecheckProgram :: Program -> Env TCValue -> Maybe TCValue
typecheckProgram (Program exp) =
  typecheckExpression exp

typecheckExpression :: Exp -> Env TCValue -> Maybe TCValue
typecheckExpression (Id x) env =
  load x env
typecheckExpression (Abs x t b) env = do
  (returnType, proof) <- typecheckExpression b $ save x (t, P.TruthHypothesis T.Int) env
  Just $ (T.Arrow t returnType, P.Abstraction t proof)
typecheckExpression (App x y) env = do
  (xType, xProof) <- typecheckExpression x env
  (yType, yProof) <- typecheckExpression y env
  case xType of
    (T.Arrow l r) ->
      if yType == l then Just (r, P.Application xProof yProof) else Nothing
    _ -> Nothing
typecheckExpression (Number n) _ = Just (T.Int, P.TruthHypothesis T.Int)
typecheckExpression (Boolean b) _ = Just (T.Bool, P.TruthHypothesis T.Bool)
