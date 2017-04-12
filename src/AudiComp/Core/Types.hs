module AudiComp.Core.Types where

import AudiComp.Core.Env

class Pretty a where
    pretty :: a -> String

data Type
  = Int
  | Bool
  | Arrow Type Type
  | Box (Env Trail) Proof Type
  | Audited Type
  | TrailReplacement Type
  deriving (Eq, Show)

instance Pretty Type where
  pretty Int = "int"
  pretty Bool = "bool"
  pretty (Arrow l r) = pretty l ++ " -> " ++ pretty r
  pretty (Box _ p t) = "[[" ++ pretty p ++ "]]" ++ pretty t
  pretty (Audited t) = pretty t ++ " audited"
  pretty (TrailReplacement t) = "trl " ++ pretty t

data Proof
    = TruthHypothesis Type
    | ConstantInt Int
    | ConstantBool Bool
    | Abstraction Type Proof
    | Application Proof Proof
    | ValidityHypothesis String String String
    | BoxIntroduction (Env Trail) Proof
    | BoxElimination Type Proof Proof
    | TrailInspectionP String Proof Proof Proof Proof Proof Proof Proof Proof Proof Proof
    deriving (Eq, Show)

instance Pretty Proof where
  pretty (TruthHypothesis t) = pretty t
  pretty (ConstantInt i) = show i
  pretty (ConstantBool b) = show b
  pretty (Abstraction t p) = "fun " ++ pretty t ++ " -> " ++ pretty p
  pretty (Application p1 p2) = pretty p1 ++ " . " ++ pretty p2
  pretty (ValidityHypothesis u oldName newName) = "<" ++ u ++ ";" ++ oldName ++ "/" ++ newName ++ ">"
  pretty (BoxIntroduction _ p) = "SIGMA . " ++ pretty p
  pretty (BoxElimination t p1 p2) = "LET(u:" ++ pretty t ++ "." ++ pretty p1 ++ "," ++ pretty p2 ++ ")"
  pretty (TrailInspectionP trail p1 p2 p3 p4 p5 p6 p7 p8 p9 p10) =
      trail ++ "[\n"
        ++ "r -> " ++ pretty p1 ++ "\n"
        ++ "s -> " ++ pretty p2 ++ "\n"
        ++ "t -> " ++ pretty p3 ++ "\n"
        ++ "ba -> " ++ pretty p4 ++ "\n"
        ++ "bb -> " ++ pretty p5 ++ "\n"
        ++ "ti -> " ++ pretty p6 ++ "\n"
        ++ "abs -> " ++ pretty p7 ++ "\n"
        ++ "app -> " ++ pretty p8 ++ "\n"
        ++ "let -> " ++ pretty p9 ++ "\n"
        ++ "trpl -> " ++ pretty p10 ++ "\n"
        ++ "]"

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
