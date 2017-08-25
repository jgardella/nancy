module AudiComp.Core.Util where

import qualified AudiComp.Core.Language as L
import AudiComp.Core.Errors.Interpreter as Err
import AudiComp.Core.Env
import qualified Data.Map as Map
import Data.List
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader

data ValidityVarSubParams =
  ValidityVarSubParams {
      u :: String
    , trailEnv :: Env L.Trail
    , p :: L.Witness
  }

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all (== x) xs

unzipTrailRenames :: [L.TrailRename] -> ([String], [String])
unzipTrailRenames = unzip . fmap toPair
  where toPair L.TrailRename { L.old=old, L.new=new } = (old, new)

renameTrailVars :: [L.TrailRename] -> L.Trail -> L.Trail
renameTrailVars params (L.Reflexivity p) =
  L.Reflexivity (renameWitnessTrailVars params p)
renameTrailVars params (L.Symmetry e) =
  L.Symmetry (renameTrailVars params e)
renameTrailVars params (L.Transitivity e1 e2) =
  L.Transitivity
    (renameTrailVars params e1)
    (renameTrailVars params e2)
renameTrailVars params (L.Beta t p1 p2) =
  L.Beta
    (renameTypeTrailVars params t)
    (renameWitnessTrailVars params p1)
    (renameWitnessTrailVars params p2)
renameTrailVars params (L.BetaBox u t p1 p2) =
  L.BetaBox
    u
    (renameTypeTrailVars params t)
    (renameWitnessTrailVars params p1)
    (renameWitnessTrailVars params p2)
renameTrailVars params (L.AbsCompat t e) =
  L.AbsCompat
    (renameTypeTrailVars params t)
    (renameTrailVars params e)
renameTrailVars params (L.AppCompat e1 e2) =
  L.AppCompat
    (renameTrailVars params e1)
    (renameTrailVars params e2)
renameTrailVars params (L.LetCompat u t e1 e2) =
  L.LetCompat
    u
    (renameTypeTrailVars params t)
    (renameTrailVars params e1)
    (renameTrailVars params e2)
renameTrailVars params
  (L.TrailInspectionT s e1 e2 e3 e4 e5 e6 e7 e8 e9 e10) =
  L.TrailInspectionT
    s
    (renameTrailVars params e1)
    (renameTrailVars params e2)
    (renameTrailVars params e3)
    (renameTrailVars params e4)
    (renameTrailVars params e5)
    (renameTrailVars params e6)
    (renameTrailVars params e7)
    (renameTrailVars params e8)
    (renameTrailVars params e9)
    (renameTrailVars params e10)

renameEnvTrailVars :: [L.TrailRename] -> Env L.Trail -> Env L.Trail
renameEnvTrailVars params =
   Map.map (renameTrailVars params)

renameTrailVar :: [L.TrailRename] -> String -> String
renameTrailVar trailRenames oldTrailVar =
  let matchedRename = find (\L.TrailRename { L.old=old } -> old == oldTrailVar) trailRenames
  in
    case matchedRename of
      Just L.TrailRename { L.new=new } -> new
      Nothing -> oldTrailVar

renameWitnessTrailVars :: [L.TrailRename] -> L.Witness -> L.Witness
renameWitnessTrailVars params (L.TruthHypothesisW t) =
  L.TruthHypothesisW (renameTypeTrailVars params t)
renameWitnessTrailVars _ (L.ConstantIntW n) = L.ConstantIntW n
renameWitnessTrailVars _ (L.ConstantBoolW b) = L.ConstantBoolW b
renameWitnessTrailVars params (L.AbstractionW t p) =
  L.AbstractionW (renameTypeTrailVars params t) (renameWitnessTrailVars params p)
renameWitnessTrailVars params (L.ApplicationW p1 p2) =
  L.ApplicationW (renameWitnessTrailVars params p1) (renameWitnessTrailVars params p2)
renameWitnessTrailVars _ (L.ValidityHypothesisW s1 s2) = L.ValidityHypothesisW s1 s2
renameWitnessTrailVars params (L.BoxIntroductionW trailEnv p) =
  L.BoxIntroductionW (renameEnvTrailVars params trailEnv) (renameWitnessTrailVars params p)
renameWitnessTrailVars params (L.BoxEliminationW u t p1 p2) =
  L.BoxEliminationW
    u
    (renameTypeTrailVars params t)
    (renameWitnessTrailVars params p1)
    (renameWitnessTrailVars params p2)
renameWitnessTrailVars params
  (L.TrailInspectionW s p1 p2 p3 p4 p5 p6 p7 p8 p9 p10) =
  let newS = renameTrailVar params s in
    L.TrailInspectionW
      newS
      (renameWitnessTrailVars params p1)
      (renameWitnessTrailVars params p2)
      (renameWitnessTrailVars params p3)
      (renameWitnessTrailVars params p4)
      (renameWitnessTrailVars params p5)
      (renameWitnessTrailVars params p6)
      (renameWitnessTrailVars params p7)
      (renameWitnessTrailVars params p8)
      (renameWitnessTrailVars params p9)
      (renameWitnessTrailVars params p10)

renameTypeTrailVars :: [L.TrailRename] -> L.Type -> L.Type
renameTypeTrailVars _ L.IntT = L.IntT
renameTypeTrailVars _ L.BoolT = L.BoolT
renameTypeTrailVars params (L.ArrowT l r) =
  L.ArrowT (renameTypeTrailVars params l) (renameTypeTrailVars params r)
renameTypeTrailVars params (L.BoxT s trailEnv p t) =
  L.BoxT
    s
    (renameEnvTrailVars params trailEnv)
    (renameWitnessTrailVars params p)
    (renameTypeTrailVars params t)
renameTypeTrailVars params (L.TrailReplacementT t) =
  L.TrailReplacementT (renameTypeTrailVars params t)

subsituteTypeValidityVars :: ValidityVarSubParams -> L.Type -> L.Type
subsituteTypeValidityVars _ L.IntT = L.IntT
subsituteTypeValidityVars _ L.BoolT = L.BoolT
subsituteTypeValidityVars params (L.ArrowT l r) =
  L.ArrowT
    (subsituteTypeValidityVars params l)
    (subsituteTypeValidityVars params r)
subsituteTypeValidityVars params (L.BoxT s boxEnv boxP boxT) =
  L.BoxT
    s
    boxEnv
    (subsituteWitnessValidityVars params boxP)
    (subsituteTypeValidityVars params boxT)
subsituteTypeValidityVars params (L.TrailReplacementT t) =
  L.TrailReplacementT (subsituteTypeValidityVars params t)

subsituteWitnessValidityVars :: ValidityVarSubParams -> L.Witness -> L.Witness
subsituteWitnessValditiyVars _ (L.TruthHypothesisW t) =
  L.TruthHypothesisW t
subsituteWitnessValidityVars _ (L.ConstantIntW n) =
  L.ConstantIntW n
subsituteWitnessValidityVars _ (L.ConstantBoolW b) =
  L.ConstantBoolW b
subsituteWitnessValidityVars params (L.AbstractionW t p) =
  L.AbstractionW
    t
    (subsituteWitnessValidityVars params p)
subsituteWitnessValidityVars params (L.ApplicationW p1 p2) =
  L.ApplicationW
    (subsituteWitnessValidityVars params p1)
    (subsituteWitnessValidityVars params p2)
subsituteWitnessValidityVars
  ValidityVarSubParams{u=u, p=p, trailEnv=trailEnv}
  (L.ValidityHypothesisW s1 trailRenames)
   | s1 == u =
    renameWitnessTrailVars trailRenames p
   | otherwise =
    L.ValidityHypothesisW s1 trailRenames
subsituteWitnessValidityVars params (L.BoxIntroductionW trailEnv p) =
  L.BoxIntroductionW
   trailEnv
   (subsituteWitnessValidityVars params p)
subsituteWitnessValidityVars params (L.BoxEliminationW u t p1 p2) =
  L.BoxEliminationW
    u
    t
    (subsituteWitnessValidityVars params p1)
    (subsituteWitnessValidityVars params p2)
subsituteWitnessValidityVars params
  (L.TrailInspectionW e p1 p2 p3 p4 p5 p6 p7 p8 p9 p10) =
  L.TrailInspectionW
    e
    (subsituteWitnessValidityVars params p1)
    (subsituteWitnessValidityVars params p2)
    (subsituteWitnessValidityVars params p3)
    (subsituteWitnessValidityVars params p4)
    (subsituteWitnessValidityVars params p5)
    (subsituteWitnessValidityVars params p6)
    (subsituteWitnessValidityVars params p7)
    (subsituteWitnessValidityVars params p8)
    (subsituteWitnessValidityVars params p9)
    (subsituteWitnessValidityVars params p10)

getSource :: L.Trail -> L.Witness
getSource (L.Reflexivity witness) =
  witness
getSource (L.Symmetry trl) =
  getSource trl
getSource (L.Transitivity trl1 trl2) =
  getSource trl1
getSource (L.Beta typ wit1 wit2) =
  L.ApplicationW (L.AbstractionW typ wit1) wit2
getSource (L.BetaBox u typ wit1 wit2) =
  L.BoxEliminationW u typ wit1 wit2
getSource (L.AbsCompat typ trl) =
  L.AbstractionW typ (getSource trl)
getSource (L.AppCompat trl1 trl2) =
  L.ApplicationW (getSource trl1) (getSource trl2)
getSource (L.LetCompat u typ trl1 trl2) =
  L.BoxEliminationW u typ (getSource trl1) (getSource trl2)
getSource (L.TrailInspectionT
    s
    rTrl
    sTrl
    tTrl
    baTrl
    bbTrl
    tiTrl
    absTrl
    appTrl
    letTrl
    trplTrl) =
  L.TrailInspectionW
    s
    (getSource rTrl)
    (getSource sTrl)
    (getSource tTrl)
    (getSource baTrl)
    (getSource bbTrl)
    (getSource tiTrl)
    (getSource absTrl)
    (getSource appTrl)
    (getSource letTrl)
    (getSource trplTrl)

computeWitness :: L.Exp -> ReaderT L.InterpretEnv (ExceptT InterpreterE Identity) L.Witness
computeWitness (L.Number n) =
  return $ L.ConstantIntW n
computeWitness (L.Boolean b) =
  return $ L.ConstantBoolW b
computeWitness (L.Brack exp) =
  computeWitness exp
computeWitness (L.Id x) = do
  (tEnv, _, _) <- ask
  (_, w) <- loadE x (Err.TruthVarUndefined x) tEnv
  return w
computeWitness (L.Abs x t b) = do
  bodyWitness <- computeWitness b
  return $ L.AbstractionW t bodyWitness
computeWitness (L.App x y) = do
  xWitness <- computeWitness x
  yWitness <- computeWitness y
  return $ L.ApplicationW xWitness yWitness
computeWitness (L.AuditedVar trailRenames u) =
  return $ L.ValidityHypothesisW u trailRenames
computeWitness (L.AuditedUnit trailVar exp) = do
  (_, _, eEnv) <- ask
  trail <- loadE trailVar (Err.TrailVarUndefined trailVar) eEnv
  return $ L.BoxIntroductionW eEnv (getSource trail)
computeWitness (L.AuditedComp u typ arg body) = do
  argWitness <- computeWitness arg
  bodyWitness <- computeWitness body
  return $ L.BoxEliminationW u typ bodyWitness argWitness
computeWitness
  (L.TrailInspect trailVar
    (L.ReflexivityM exp_r)
    (L.SymmetryM s1 exp_s)
    (L.TransitivityM t1 t2 exp_t)
    (L.BetaM exp_ba)
    (L.BetaBoxM exp_bb)
    (L.TrailInspectionM exp_ti)
    (L.AbstractionM abs1 exp_abs)
    (L.ApplicationM app1 app2 exp_app)
    (L.LetM let1 let2 exp_let)
    (L.ReplacementM e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 exp_e))
    = do
  rWitness <- computeWitness exp_r
  sWitness <- computeWitness exp_s
  tWitness <- computeWitness exp_t
  baWitness <- computeWitness exp_ba
  bbWitness <- computeWitness exp_bb
  tiWitness <- computeWitness exp_ti
  absWitness <- computeWitness exp_abs
  appWitness <- computeWitness exp_app
  letWitness <- computeWitness exp_let
  trplWitness <- computeWitness exp_e
  return $ L.TrailInspectionW
    trailVar
    rWitness
    sWitness
    tWitness
    baWitness
    bbWitness
    tiWitness
    absWitness
    appWitness
    letWitness
    trplWitness
