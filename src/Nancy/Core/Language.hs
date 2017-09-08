module Nancy.Core.Language where

import Nancy.Core.Env
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

data Type
  = IntT
  | BoolT
  | ArrowT Type Type
  | BoxT String (Env Trail) Witness Type
  | TrailReplacementT Type
  deriving (Eq, Show)

instance Pretty Type where
  pPrint IntT = text "int"
  pPrint BoolT = text "bool"
  pPrint (ArrowT l r) = pPrint l <+> text "->" <+> pPrint r
  pPrint (BoxT _ e p t) = text "[" <> pPrint e <+> text "." <+> pPrint p <> text "]" <+> pPrint t
  pPrint (TrailReplacementT t) = text "trl" <+> pPrint t

data Witness
  = TruthHypothesisW String
  | ConstantIntW Int
  | ConstantBoolW Bool
  | AbstractionW Type Witness
  | ApplicationW Witness Witness
  | ValidityHypothesisW String [TrailRename]
  | BoxIntroductionW (Env Trail) Witness
  | BoxEliminationW String Type Witness Witness
  | TrailInspectionW String Witness Witness Witness Witness Witness Witness Witness Witness Witness
  deriving (Eq, Show)

instance Pretty Witness where
  pPrint (TruthHypothesisW s) = text s
  pPrint (ConstantIntW i) = text (show i)
  pPrint (ConstantBoolW b) = text (show b)
  pPrint (AbstractionW t p) =
      vcat [ text "fun" <+> pPrint t <+> text "->"
           , nest 2 (pPrint p)]
  pPrint (ApplicationW p1 p2) = pPrint p1 <> text " . " <> pPrint p2
  pPrint (ValidityHypothesisW u trailRenames) = text "<" <> text u <> text ";" <+> pPrint trailRenames <> text ">"
  pPrint (BoxIntroductionW e p) = pPrint e <+> text "." <+> pPrint p
  pPrint (BoxEliminationW u t p1 p2) =
    vcat [ text "LET("
         , nest 2 (text u <> text ":" <> pPrint t <+> text "." <> pPrint p1 <> text ",")
         , nest 2 (pPrint p2 <> text ")")]
  pPrint (TrailInspectionW s p1 p2 p3 p4 p5 p6 p7 p8 p9) =
    vcat [ text "r ->" <+> pPrint p1
         , text "s ->" <+> pPrint p2
         , text "t ->" <+> pPrint p3
         , text "ba ->" <+> pPrint p4
         , text "bb ->" <+> pPrint p5
         , text "ti ->" <+> pPrint p6
         , text "abs ->" <+> pPrint p7
         , text "app ->" <+> pPrint p8
         , text "let ->" <+> pPrint p9]

data Trail
    = EmptyT
    | Reflexivity Witness
    | Symmetry Trail
    | Transitivity Trail Trail
    | Beta Type Witness Witness
    | BetaBox String Type Witness Witness
    | AbsCompat Type Trail
    | AppCompat Trail Trail
    | LetCompat String Type Trail Trail
    | TrailInspectionT String Witness Witness Witness Witness Witness Witness Witness Witness Witness
    deriving (Eq, Show)

instance Pretty Trail where
  pPrint EmptyT = text "emptyT"
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
  pPrint (BetaBox u t w1 w2) =
    vcat [ text "bb("
         , nest 2 (text u <> text ":" <> pPrint t <+> text "." <> pPrint w1 <> text ",")
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
  pPrint (LetCompat u t e1 e2) =
    vcat [ text "let("
         , nest 2 (text u <> text ":" <> pPrint t <+> text "." <> pPrint e1 <> text ",")
         , nest 2 (pPrint e2)
         , text ")"]
  pPrint (TrailInspectionT s w1 w2 w3 w4 w5 w6 w7 w8 w9) =
    vcat [ text "ti(" <> text s <> text ","
         , nest 2 (pPrint w1 <> text ",")
         , nest 2 (pPrint w2 <> text ",")
         , nest 2 (pPrint w3 <> text ",")
         , nest 2 (pPrint w4 <> text ",")
         , nest 2 (pPrint w5 <> text ",")
         , nest 2 (pPrint w6 <> text ",")
         , nest 2 (pPrint w7 <> text ",")
         , nest 2 (pPrint w8 <> text ",")
         , nest 2 (pPrint w9 )
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
  | AuditedVar [TrailRename] String
  | AuditedUnit String Exp
  | AuditedComp String Type Exp Exp
  | TrailInspect String ReflexivityM SymmetryM TransitivityM BetaM BetaBoxM TrailInspectionM AbstractionM ApplicationM LetM
  deriving (Eq, Show)

newtype ReflexivityM = ReflexivityM Exp
  deriving (Eq, Show)
data SymmetryM = SymmetryM String Exp
  deriving (Eq, Show)
data TransitivityM = TransitivityM String String Exp
  deriving (Eq, Show)
newtype BetaM = BetaM Exp
  deriving (Eq, Show)
newtype BetaBoxM = BetaBoxM Exp
  deriving (Eq, Show)
newtype TrailInspectionM = TrailInspectionM Exp
  deriving (Eq, Show)
data AbstractionM = AbstractionM String Exp
  deriving (Eq, Show)
data ApplicationM = ApplicationM String String Exp
  deriving (Eq, Show)
data LetM = LetM String String Exp
  deriving (Eq, Show)

data TrailRename = TrailRename {
    old :: String
  , new :: String
  } deriving (Show, Eq)

instance Pretty TrailRename where
  pPrint TrailRename { old=old, new=new } =
    text (old ++ "->" ++ new)

data Value
  = IntV Int
  | BoolV Bool
  | ArrowV InterpretEnv String Type Exp
  | BoxV String (Env Trail) Witness Value
  deriving (Eq, Show)

instance Pretty Value where
  pPrint (IntV i) = int i
  pPrint (BoolV b) = text $ show b
  pPrint (ArrowV _ arg typ body) = text "(" <> text arg <> text ":" <> pPrint typ <+> text "->" <+> text (show body) <+> text ")"
  pPrint (BoxV _ trailEnv witness value) =
    text "[" <> pPrint trailEnv <+> text "." <+> pPrint witness <> text "]" <+> pPrint value

type ValuePair = (Value, Trail)

type InterpretEnv = (Env Value, Env Value, Env Trail)
