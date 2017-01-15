module Proof where

import qualified Types as T

data Proof
    = TruthHypothesis T.Type
    | Abstraction T.Type Proof
    | Application Proof Proof
--    | ValidityHypothesis u sigma
--    | BoxIntroduction x Proof
--    | BoxElimination u Proof Proof
--    | TrailInspection
--
instance Show Proof where
  show (TruthHypothesis t) = show t
  show (Abstraction t p) = "fun a : " ++ show t ++ " . { " ++ show p ++ " }"
  show (Application p1 p2) = "(" ++ show p1 ++ " . " ++ show p2 ++ ")"

