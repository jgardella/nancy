module AudiComp.Core.Util where

import AudiComp.Core.Language as L
import AudiComp.Core.Env
import qualified Data.Map as Map

data ValidityVarSubParams =
  ValidityVarSubParams {
      u :: String
    , trailEnv :: Env Trail
    , p :: L.Witness
  }

data RenameTrailVarsParams =
  RenameTrailVarsParams {
      old :: String
    , new :: String
  }

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all (== x) xs

renameTrailVars :: RenameTrailVarsParams -> Trail -> Trail
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
renameTrailVars params (L.BetaBox t p1 p2) =
  L.BetaBox
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
renameTrailVars params (L.LetCompat t e1 e2) =
  L.LetCompat
    (renameTypeTrailVars params t)
    (renameTrailVars params e1)
    (renameTrailVars params e2)
renameTrailVars params
  (TrailInspectionT e1 e2 e3 e4 e5 e6 e7 e8 e9 e10) =
  TrailInspectionT
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

renameEnvTrailVars :: RenameTrailVarsParams -> Env Trail -> Env Trail
renameEnvTrailVars params =
   Map.map (renameTrailVars params)

renameWitnessTrailVars :: RenameTrailVarsParams -> L.Witness -> L.Witness
renameWitnessTrailVars params (L.TruthHypothesisW t) =
  L.TruthHypothesisW (renameTypeTrailVars params t)
renameWitnessTrailVars _ (L.ConstantIntW n) = L.ConstantIntW n
renameWitnessTrailVars _ (L.ConstantBoolW b) = L.ConstantBoolW b
renameWitnessTrailVars params (L.AbstractionW t p) =
  L.AbstractionW (renameTypeTrailVars params t) (renameWitnessTrailVars params p)
renameWitnessTrailVars params (L.ApplicationW p1 p2) =
  L.ApplicationW (renameWitnessTrailVars params p1) (renameWitnessTrailVars params p2)
renameWitnessTrailVars _ (L.ValidityHypothesisW s1 s2 s3) = L.ValidityHypothesisW s1 s2 s3
renameWitnessTrailVars params (L.BoxIntroductionW trailEnv p) =
  L.BoxIntroductionW (renameEnvTrailVars params trailEnv) (renameWitnessTrailVars params p)
renameWitnessTrailVars params (L.BoxEliminationW t p1 p2) =
  L.BoxEliminationW
    (renameTypeTrailVars params t)
    (renameWitnessTrailVars params p1)
    (renameWitnessTrailVars params p2)
renameWitnessTrailVars params
  (L.TrailInspectionW s p1 p2 p3 p4 p5 p6 p7 p8 p9 p10) =
  let newS = (if s == old params then new params else s) in
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

renameTypeTrailVars :: RenameTrailVarsParams -> L.Type -> L.Type
renameTypeTrailVars _ L.IntT = L.IntT
renameTypeTrailVars _ L.BoolT = L.BoolT
renameTypeTrailVars params (L.ArrowT l r) =
  L.ArrowT (renameTypeTrailVars params l) (renameTypeTrailVars params r)
renameTypeTrailVars params (L.BoxT trailEnv p t) =
  L.BoxT
    (renameEnvTrailVars params trailEnv)
    (renameWitnessTrailVars params p)
    (renameTypeTrailVars params t)
renameTypeTrailVars params (L.TrailReplacementT t) =
  L.TrailReplacementT (renameTypeTrailVars params t)

subsituteTypeValidityVars :: ValidityVarSubParams -> Type -> Type
subsituteTypeValidityVars _ L.IntT = L.IntT
subsituteTypeValidityVars _ L.BoolT = L.BoolT
subsituteTypeValidityVars params (L.ArrowT l r) =
  L.ArrowT
    (subsituteTypeValidityVars params l)
    (subsituteTypeValidityVars params r)
subsituteTypeValidityVars params (L.BoxT boxEnv boxP boxT) =
  L.BoxT
    boxEnv
    (subsituteWitnessValidityVars params boxP)
    (subsituteTypeValidityVars params boxT)
subsituteTypeValidityVars params (L.TrailReplacementT t) =
  L.TrailReplacementT (subsituteTypeValidityVars params t)

subsituteWitnessValidityVars :: ValidityVarSubParams -> Witness -> Witness
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
  (L.ValidityHypothesisW s1 old new)
   | s1 == u =
    renameWitnessTrailVars RenameTrailVarsParams{old=old, new=new} p
   | otherwise =
    L.ValidityHypothesisW s1 old new
subsituteWitnessValidityVars params (L.BoxIntroductionW trailEnv p) =
  L.BoxIntroductionW
   trailEnv
   (subsituteWitnessValidityVars params p)
subsituteWitnessValidityVars params (L.BoxEliminationW t p1 p2) =
  L.BoxEliminationW
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
