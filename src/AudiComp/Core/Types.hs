module AudiComp.Core.Types where

import AudiComp.Core.Env

class Pretty a where
    pretty :: a -> String

data Type
  = IntT
  | BoolT
  | ArrowT Type Type
  | BoxT (Env Trail) Witness Type
  | AuditedT Type
  | TrailReplacementT Type
  deriving (Eq, Show)

instance Pretty Type where
  pretty IntT = "int"
  pretty BoolT = "bool"
  pretty (ArrowT l r) = pretty l ++ " -> " ++ pretty r
  pretty (BoxT _ p t) = "[[" ++ pretty p ++ "]]" ++ pretty t
  pretty (AuditedT t) = pretty t ++ " audited"
  pretty (TrailReplacementT t) = "trl " ++ pretty t

data Witness
    = TruthHypothesisW Type
    | ConstantIntW Int
    | ConstantBoolW Bool
    | AbstractionW Type Witness
    | ApplicationW Witness Witness
    | ValidityHypothesisW String String String
    | BoxIntroductionW (Env Trail) Witness
    | BoxEliminationW Type Witness Witness
    | TrailInspectionW String Witness Witness Witness Witness Witness Witness Witness Witness Witness Witness
    deriving (Eq, Show)

instance Pretty Witness where
  pretty (TruthHypothesisW t) = pretty t
  pretty (ConstantIntW i) = show i
  pretty (ConstantBoolW b) = show b
  pretty (AbstractionW t p) = "fun " ++ pretty t ++ " -> " ++ pretty p
  pretty (ApplicationW p1 p2) = pretty p1 ++ " . " ++ pretty p2
  pretty (ValidityHypothesisW u oldName newName) = "<" ++ u ++ ";" ++ oldName ++ "/" ++ newName ++ ">"
  pretty (BoxIntroductionW _ p) = "SIGMA . " ++ pretty p
  pretty (BoxEliminationW t p1 p2) = "LET(u:" ++ pretty t ++ "." ++ pretty p1 ++ "," ++ pretty p2 ++ ")"
  pretty (TrailInspectionW trail p1 p2 p3 p4 p5 p6 p7 p8 p9 p10) =
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
    = Reflexivity Witness
    | Symmetry Trail
    | Transitivity Trail Trail
    | Beta Type Witness Witness
    | BetaBox Type Witness Witness
    | AbsCompat Type Trail
    | AppCompat Trail Trail
    | LetCompat Type Trail Trail
    | TrailInspectionT Trail Trail Trail Trail Trail Trail Trail Trail Trail Trail
    deriving (Eq, Show)
