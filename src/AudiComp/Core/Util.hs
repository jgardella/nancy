module AudiComp.Core.Util where

import AudiComp.Core.Types as T
import AudiComp.Core.Env
import qualified Data.Map as Map

data ValidityVarSubParams =
  ValidityVarSubParams {
      u :: String
    , trailEnv :: Env Trail
    , p :: Proof
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
renameTrailVars params (T.Reflexivity p) =
  T.Reflexivity (renameProofTrailVars params p)
renameTrailVars params (T.Symmetry e) =
  T.Symmetry (renameTrailVars params e)
renameTrailVars params (T.Transitivity e1 e2) =
  T.Transitivity
    (renameTrailVars params e1)
    (renameTrailVars params e2)
renameTrailVars params (T.Beta t p1 p2) =
  T.Beta
    (renameTypeTrailVars params t)
    (renameProofTrailVars params p1)
    (renameProofTrailVars params p2)
renameTrailVars params (T.BetaBox t p1 p2) =
  T.BetaBox
    (renameTypeTrailVars params t)
    (renameProofTrailVars params p1)
    (renameProofTrailVars params p2)
renameTrailVars params (T.AbsCompat t e) =
  T.AbsCompat
    (renameTypeTrailVars params t)
    (renameTrailVars params e)
renameTrailVars params (T.AppCompat e1 e2) =
  T.AppCompat
    (renameTrailVars params e1)
    (renameTrailVars params e2)
renameTrailVars params (T.LetCompat t e1 e2) =
  T.LetCompat
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

renameProofTrailVars :: RenameTrailVarsParams -> T.Proof -> T.Proof
renameProofTrailVars params (T.TruthHypothesis t) =
  T.TruthHypothesis (renameTypeTrailVars params t)
renameProofTrailVars _ (T.ConstantInt n) = T.ConstantInt n
renameProofTrailVars _ (T.ConstantBool b) = T.ConstantBool b
renameProofTrailVars params (T.Abstraction t p) =
  T.Abstraction (renameTypeTrailVars params t) (renameProofTrailVars params p)
renameProofTrailVars params (T.Application p1 p2) =
  T.Application (renameProofTrailVars params p1) (renameProofTrailVars params p2)
renameProofTrailVars _ (T.ValidityHypothesis s1 s2 s3) = T.ValidityHypothesis s1 s2 s3
renameProofTrailVars params (T.BoxIntroduction trailEnv p) =
  T.BoxIntroduction (renameEnvTrailVars params trailEnv) (renameProofTrailVars params p)
renameProofTrailVars params (T.BoxElimination t p1 p2) =
  T.BoxElimination
    (renameTypeTrailVars params t)
    (renameProofTrailVars params p1)
    (renameProofTrailVars params p2)
renameProofTrailVars params
  (T.TrailInspectionP s p1 p2 p3 p4 p5 p6 p7 p8 p9 p10) =
  let newS = (if s == old params then new params else s) in
    T.TrailInspectionP
      newS
      (renameProofTrailVars params p1)
      (renameProofTrailVars params p2)
      (renameProofTrailVars params p3)
      (renameProofTrailVars params p4)
      (renameProofTrailVars params p5)
      (renameProofTrailVars params p6)
      (renameProofTrailVars params p7)
      (renameProofTrailVars params p8)
      (renameProofTrailVars params p9)
      (renameProofTrailVars params p10)

renameTypeTrailVars :: RenameTrailVarsParams -> T.Type -> T.Type
renameTypeTrailVars _ T.Int = T.Int
renameTypeTrailVars _ T.Bool = T.Bool
renameTypeTrailVars params (T.Arrow l r) =
  T.Arrow (renameTypeTrailVars params l) (renameTypeTrailVars params r)
renameTypeTrailVars params (T.Box trailEnv p t) =
  T.Box
    (renameEnvTrailVars params trailEnv)
    (renameProofTrailVars params p)
    (renameTypeTrailVars params t)
renameTypeTrailVars params (T.Audited t) =
  T.Audited (renameTypeTrailVars params t)
renameTypeTrailVars params (T.TrailReplacement t) =
  T.TrailReplacement (renameTypeTrailVars params t)

subsituteTypeValidityVars :: ValidityVarSubParams -> Type -> Type
subsituteTypeValidityVars _ T.Int = T.Int
subsituteTypeValidityVars _ T.Bool = T.Bool
subsituteTypeValidityVars params (T.Arrow l r) =
  T.Arrow
    (subsituteTypeValidityVars params l)
    (subsituteTypeValidityVars params r)
subsituteTypeValidityVars params (T.Box boxEnv boxP boxT) =
  T.Box
    boxEnv
    (subsituteProofValidityVars params boxP)
    (subsituteTypeValidityVars params boxT)
subsituteTypeValidityVars params (T.Audited t) =
  T.Audited (subsituteTypeValidityVars params t)
subsituteTypeValidityVars params (T.TrailReplacement t) =
  T.TrailReplacement (subsituteTypeValidityVars params t)

subsituteProofValidityVars :: ValidityVarSubParams -> Proof -> Proof
subsituteProofValditiyVars _ (T.TruthHypothesis t) =
  T.TruthHypothesis t
subsituteProofValidityVars _ (T.ConstantInt n) =
  T.ConstantInt n
subsituteProofValidityVars _ (T.ConstantBool b) =
  T.ConstantBool b
subsituteProofValidityVars params (T.Abstraction t p) =
  T.Abstraction
    t
    (subsituteProofValidityVars params p)
subsituteProofValidityVars params (T.Application p1 p2) =
  T.Application
    (subsituteProofValidityVars params p1)
    (subsituteProofValidityVars params p2)
subsituteProofValidityVars
  ValidityVarSubParams{u=u, p=p, trailEnv=trailEnv}
  (T.ValidityHypothesis s1 old new)
   | s1 == u =
    renameProofTrailVars RenameTrailVarsParams{old=old, new=new} p
   | otherwise =
    T.ValidityHypothesis s1 old new
subsituteProofValidityVars params (T.BoxIntroduction trailEnv p) =
  T.BoxIntroduction
   trailEnv
   (subsituteProofValidityVars params p)
subsituteProofValidityVars params (T.BoxElimination t p1 p2) =
  T.BoxElimination
    t
    (subsituteProofValidityVars params p1)
    (subsituteProofValidityVars params p2)
subsituteProofValidityVars params
  (T.TrailInspectionP e p1 p2 p3 p4 p5 p6 p7 p8 p9 p10) =
  T.TrailInspectionP
    e
    (subsituteProofValidityVars params p1)
    (subsituteProofValidityVars params p2)
    (subsituteProofValidityVars params p3)
    (subsituteProofValidityVars params p4)
    (subsituteProofValidityVars params p5)
    (subsituteProofValidityVars params p6)
    (subsituteProofValidityVars params p7)
    (subsituteProofValidityVars params p8)
    (subsituteProofValidityVars params p9)
    (subsituteProofValidityVars params p10)
