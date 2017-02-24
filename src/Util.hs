module Util where

import Types as T
import Env
import qualified Data.Map as Map

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all (== x) xs

renameTrailVars :: String -> String -> Trail -> Trail
renameTrailVars old new (T.Reflexivity p) =
  T.Reflexivity (renameProofTrailVars old new p)
renameTrailVars old new (T.Symmetry e) =
  T.Symmetry (renameTrailVars old new e)
renameTrailVars old new (T.Transitivity e1 e2) =
  T.Transitivity
    (renameTrailVars old new e1)
    (renameTrailVars old new e2)
renameTrailVars old new (T.Beta t p1 p2) =
  T.Beta
    (renameTypeTrailVars old new t)
    (renameProofTrailVars old new p1)
    (renameProofTrailVars old new p2)
renameTrailVars old new (T.BetaBox t p1 p2) =
  T.BetaBox
    (renameTypeTrailVars old new t)
    (renameProofTrailVars old new p1)
    (renameProofTrailVars old new p2)
renameTrailVars old new (T.AbsCompat t e) =
  T.AbsCompat
    (renameTypeTrailVars old new t)
    (renameTrailVars old new e)
renameTrailVars old new (T.AppCompat e1 e2) =
  T.AppCompat
    (renameTrailVars old new e1)
    (renameTrailVars old new e2)
renameTrailVars old new (T.LetCompat t e1 e2) =
  T.LetCompat
    (renameTypeTrailVars old new t)
    (renameTrailVars old new e1)
    (renameTrailVars old new e2)
renameTrailVars old new
  (TrailInspectionT e1 e2 e3 e4 e5 e6 e7 e8 e9 e10) =
  TrailInspectionT
    (renameTrailVars old new e1)
    (renameTrailVars old new e2)
    (renameTrailVars old new e3)
    (renameTrailVars old new e4)
    (renameTrailVars old new e5)
    (renameTrailVars old new e6)
    (renameTrailVars old new e7)
    (renameTrailVars old new e8)
    (renameTrailVars old new e9)
    (renameTrailVars old new e10)

renameEnvTrailVars :: String -> String -> Env Trail -> Env Trail
renameEnvTrailVars old new trailEnv =
   Map.map (renameTrailVars old new) trailEnv

renameProofTrailVars :: String -> String -> T.Proof -> T.Proof
renameProofTrailVars old new (T.TruthHypothesis t) =
  T.TruthHypothesis (renameTypeTrailVars old new t)
renameProofTrailVars _ _ (T.ConstantInt n) = T.ConstantInt n
renameProofTrailVars _ _ (T.ConstantBool b) = T.ConstantBool b
renameProofTrailVars old new (T.Abstraction t p) =
  T.Abstraction (renameTypeTrailVars old new t) (renameProofTrailVars old new p)
renameProofTrailVars old new (T.Application p1 p2) =
  T.Application (renameProofTrailVars old new p1) (renameProofTrailVars old new p2)
renameProofTrailVars _ _ (T.ValidityHypothesis s1 s2) = T.ValidityHypothesis s1 s2
renameProofTrailVars old new (T.BoxIntroduction trailEnv p) =
  T.BoxIntroduction (renameEnvTrailVars old new trailEnv) (renameProofTrailVars old new p)
renameProofTrailVars old new (T.BoxElimination t p1 p2) =
  T.BoxElimination
    (renameTypeTrailVars old new t)
    (renameProofTrailVars old new p1)
    (renameProofTrailVars old new p2)
renameProofTrailVars old new
  (T.TrailInspectionP s p1 p2 p3 p4 p5 p6 p7 p8 p9 p10) =
  let newS = (if s == old then new else s) in
    T.TrailInspectionP
      newS
      (renameProofTrailVars old new p1)
      (renameProofTrailVars old new p2)
      (renameProofTrailVars old new p3)
      (renameProofTrailVars old new p4)
      (renameProofTrailVars old new p5)
      (renameProofTrailVars old new p6)
      (renameProofTrailVars old new p7)
      (renameProofTrailVars old new p8)
      (renameProofTrailVars old new p9)
      (renameProofTrailVars old new p10)

renameTypeTrailVars :: String -> String -> T.Type -> T.Type
renameTypeTrailVars _ _ T.Int = T.Int
renameTypeTrailVars _ _ T.Bool = T.Bool
renameTypeTrailVars old new (T.Arrow l r) =
  T.Arrow (renameTypeTrailVars old new l) (renameTypeTrailVars old new r)
renameTypeTrailVars old new (T.Box trailEnv p t) =
  T.Box
    (renameEnvTrailVars old new trailEnv)
    (renameProofTrailVars old new p)
    (renameTypeTrailVars old new t)
renameTypeTrailVars old new (T.Audited t) =
  T.Audited (renameTypeTrailVars old new t)
renameTypeTrailVars old new (T.TrailReplacement t) =
  T.TrailReplacement (renameTypeTrailVars old new t)
