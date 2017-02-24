module Types where

import Env

data Type
  = Int
  | Bool
  | Arrow Type Type
  | Box (Env Trail) Proof Type
  | Audited Type
  | TrailReplacement Type
  deriving (Eq, Show)

--instance Show Type where
--  show Int = "int"
--  show Bool = "bool"
--  show (Arrow l r) = show l ++ " -> " ++ show r

data Proof
    = TruthHypothesis Type
    | ConstantInt Int
    | ConstantBool Bool
    | Abstraction Type Proof
    | Application Proof Proof
    | ValidityHypothesis String String
    | BoxIntroduction (Env Trail) Proof
    | BoxElimination Type Proof Proof
    | TrailInspectionP String Proof Proof Proof Proof Proof Proof Proof Proof Proof Proof
    deriving (Eq, Show)

--instance Show Proof where
--  show (TruthHypothesis t) = show t
--  show (Abstraction t p) = "fun a : " ++ show t ++ " . { " ++ show p ++ " }"
--  show (Application p1 p2) = "(" ++ show p1 ++ " . " ++ show p2 ++ ")"

data Trail
    = Reflexivity Proof
    | Symmetry Trail
    | Transitivity Trail Trail
    | Beta Type Proof Proof
    | BetaBox Type Proof Proof
    | AbsCompat Type Trail
    | AppCompat Trail Trail
    | LetCompat Type Trail Trail
    | TrailInspectionT Trail Trail Trail Trail Trail Trail Trail Trail Trail Trail
    deriving (Eq, Show)
