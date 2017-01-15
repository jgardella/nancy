module Typechecker where

import Parser
import qualified Types as T
import Context.Truth

typecheckProgram :: Program -> TruthContext T.Type -> Maybe T.Type
typecheckProgram (Program exp) =
  typecheckExpression exp

typecheckExpression :: Exp -> TruthContext T.Type -> Maybe T.Type
typecheckExpression (Id x) truth =
  load x truth
typecheckExpression (Abs x t b) truth = do
  returnType <- typecheckExpression b $ save x t truth
  Just $ T.Arrow t returnType
typecheckExpression (App x y) truth = do
  xType <- typecheckExpression x truth
  yType <- typecheckExpression y truth
  case xType of
    (T.Arrow l r) ->
      if yType == l then Just r else Nothing
    _ -> Nothing
typecheckExpression (Number n) _ = Just T.Int
typecheckExpression (Boolean b) _ = Just T.Bool
