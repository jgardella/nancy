module AudiComp.Core.Language where

import AudiComp.Core.Env
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

data Type
  = IntT
  | BoolT
  | ArrowT Type Type
  | BoxT (Env Trail) Witness Type
  | AuditedT Type
  | TrailReplacementT Type
  deriving (Eq, Show)

instance Pretty Type where
  pPrint IntT = text "int"
  pPrint BoolT = text "bool"
  pPrint (ArrowT l r) = pPrint l <+> text "->" <+> pPrint r
  pPrint (BoxT _ p t) = text "[" <> pPrint p <> text "]" <> pPrint t
  pPrint (AuditedT t) = pPrint t <+> text "audited"
  pPrint (TrailReplacementT t) = text "trl" <+> pPrint t

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
  pPrint (TruthHypothesisW t) = pPrint t
  pPrint (ConstantIntW i) = text (show i)
  pPrint (ConstantBoolW b) = text (show b)
  pPrint (AbstractionW t p) =
      vcat [ text "fun" <+> pPrint t <+> text "->"
           , nest 2 (pPrint p)]
  pPrint (ApplicationW p1 p2) = pPrint p1 <> text " . " <> pPrint p2
  pPrint (ValidityHypothesisW u oldName newName) = text "<" <> text u <> text ";" <> text oldName <> text "/" <> text newName <> text ">"
  pPrint (BoxIntroductionW _ p) = text "S ." <+> pPrint p
  pPrint (BoxEliminationW t p1 p2) =
    vcat [ text "LET("
         , nest 2 (text "u:" <> pPrint t <> text "." <> pPrint p1 <> text ",")
         , nest 2 (pPrint p2 <> text ")")]
  pPrint (TrailInspectionW trail p1 p2 p3 p4 p5 p6 p7 p8 p9 p10) =
    vcat [ text (trail ++ "[")
         , nest 2 (text "r ->" <+> pPrint p1)
         , nest 2 (text "s ->" <+> pPrint p2)
         , nest 2 (text "t ->" <+> pPrint p3)
         , nest 2 (text "ba ->" <+> pPrint p4)
         , nest 2 (text "bb ->" <+> pPrint p5)
         , nest 2 (text "ti ->" <+> pPrint p6)
         , nest 2 (text "abs ->" <+> pPrint p7)
         , nest 2 (text "app ->" <+> pPrint p8)
         , nest 2 (text "let ->" <+> pPrint p9)
         , nest 2 (text "trpl ->" <+> pPrint p10)
         , text "]"]

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

instance Pretty Trail where
  pPrint (Reflexivity w) = text "r(" <> pPrint w <> text ")"
  pPrint (Symmetry t) =
    vcat [ text "s("
         , nest 2 (pPrint t)
         , text ")"]
  pPrint (Transitivity e1 e2) =
    vcat [ text "t("
         , nest 2 (pPrint e1 <> text ",")
         , nest 2 (pPrint e2)
         , text ")"]
  pPrint (Beta t w1 w2) =
    vcat [ text "ba("
         , nest 2 (text "a:" <> pPrint t <+> text "." <> pPrint w1 <> text ",")
         , nest 2 (pPrint w2)
         , text ")"]
  pPrint (BetaBox t w1 w2) =
    vcat [ text "bb("
         , nest 2 (text "u:" <> pPrint t <+> text "." <> pPrint w1 <> text ",")
         , nest 2 (pPrint w2)
         , text ")"]
  pPrint (AbsCompat t e) =
    vcat [ text "abs("
         , nest 2 (text "a:" <> pPrint t <+> text "." <> pPrint e)
         , text ")"]
  pPrint (AppCompat e1 e2) =
    vcat [ text "app("
         , nest 2 (pPrint e1 <> text ",")
         , nest 2 (pPrint e2)
         , text ")"]
  pPrint (LetCompat t e1 e2) =
    vcat [ text "let("
         , nest 2 (text "u:" <> pPrint t <+> text "." <> pPrint e1 <> text ",")
         , nest 2 (pPrint e2)
         , text ")"]
  pPrint (TrailInspectionT e1 e2 e3 e4 e5 e6 e7 e8 e9 e10) =
    vcat [ text "trpl("
         , nest 2 (pPrint e1 <> text ",")
         , nest 2 (pPrint e2 <> text ",")
         , nest 2 (pPrint e3 <> text ",")
         , nest 2 (pPrint e4 <> text ",")
         , nest 2 (pPrint e5 <> text ",")
         , nest 2 (pPrint e6 <> text ",")
         , nest 2 (pPrint e7 <> text ",")
         , nest 2 (pPrint e8 <> text ",")
         , nest 2 (pPrint e9 <> text ",")
         , nest 2 (pPrint e10 )
         , text ")"]

newtype Program = Program Exp
  deriving Show

data Exp
  = Id String
  | Number Int
  | Boolean Bool
  | Brack Exp
  | Abs String Type Exp
  | App Exp Exp
  | AuditedVar String String String
  | AuditedUnit String Exp
  | AuditedComp String Exp Exp
  | TrailInspect String TrailMap TrailMap TrailMap TrailMap TrailMap TrailMap TrailMap TrailMap TrailMap TrailMap
  | DerivedTerm Trail Exp
  deriving Show

data TrailMap
  = ReflexivityM Exp
  | SymmetryM String Exp
  | TransitivityM String String Exp
  | BetaM Exp
  | BetaBoxM Exp
  | TrailInspectionM Exp
  | AbstractionM String Exp
  | ApplicationM String String Exp
  | LetM String String Exp
  | ReplacementM String String String String String String String String String String Exp
  deriving Show
