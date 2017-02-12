module Proof where

import qualified Types as T

data Proof
    = TruthHypothesis T.Type
    | ConstantInt Number
    | ConstantBool Boolean
    | Abstraction T.Type Proof
    | Application Proof Proof
    | ValidityHypothesis T.Type String
    | BoxIntroduction String Proof
    | BoxElimination T.Type Proof Proof
    | TrailInspection Proof
    deriving (Eq, Show)

--instance Show Proof where
--  show (TruthHypothesis t) = show t
--  show (Abstraction t p) = "fun a : " ++ show t ++ " . { " ++ show p ++ " }"
--  show (Application p1 p2) = "(" ++ show p1 ++ " . " ++ show p2 ++ ")"

