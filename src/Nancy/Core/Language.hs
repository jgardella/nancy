module Nancy.Core.Language where

import Nancy.Core.Env
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

data Type
  = IntType
  | BoolType
  | ArrowType Type Type
  | BoxType Witness Type
  deriving (Eq, Show)

instance Pretty Type where
  pPrint IntType = text "int"
  pPrint BoolType = text "bool"
  pPrint (ArrowType l r) = pPrint l <+> text "->" <+> pPrint r
  pPrint (BoxType p t) = text "[" <> pPrint p <> text "]" <+> pPrint t

data Witness
  = VarWit String
  | IntWit Int
  | BoolWit Bool
  | LamWit String Type Witness
  | AppWit Witness Witness
  | AVarWit String
  | BangWit Witness
  | LetWit String Type Witness Witness
  | TiWit Witness Witness Witness Witness Witness Witness Witness Witness Witness
  deriving (Eq, Show)

instance Pretty Witness where
  pPrint (VarWit s) = text "varwit(" <> text s <> text ")"
  pPrint (IntWit i) = text "intwit(" <> text (show i) <> text ")"
  pPrint (BoolWit b) = text "boolwit(" <> text (show b) <> text ")"
  pPrint (LamWit s t p) = text "lamwit(" <> text s <> text ":" <> pPrint t <> text "," <+> pPrint p <> text ")"
  pPrint (AppWit p1 p2) = text "appwit(" <> pPrint p1 <> text ", " <> pPrint p2 <> text ")"
  pPrint (AVarWit u) = text "avarwit(" <> text u <> text ")"
  pPrint (BangWit w) = text "bangwit(" <> pPrint w <> text")"
  pPrint (LetWit u t p1 p2) =
    text "letwit(" <> text u <> text ":" <> pPrint t <> text ","
      <+> pPrint p1 <> text "," <+> pPrint p2 <> text ")"
  pPrint (TiWit rWit tWit baWit bbWit tiWit lamWit appWit letWit trplWit) =
    text "tiwit("
      <> pPrint rWit <> text ","
      <+> pPrint tWit <> text ","
      <+> pPrint baWit <> text ","
      <+> pPrint bbWit <> text ","
      <+> pPrint tiWit <> text ","
      <+> pPrint lamWit <> text ","
      <+> pPrint appWit <> text ","
      <+> pPrint letWit <> text ","
      <+> pPrint trplWit <> text ")"

data Trail
    = EmptyT
    | RTrail Witness
    | TTrail Trail Trail
    | BaTrail String Type Witness Witness
    | BbTrail String Type Witness Witness
    | TiTrail Trail Witness Witness Witness Witness Witness Witness Witness Witness Witness
    | LamTrail String Type Trail
    | AppTrail Trail Trail
    | LetTrail String Type Trail Trail
    | TrplTrail Trail Trail Trail Trail Trail Trail Trail Trail Trail
    deriving (Eq, Show)

instance Pretty Trail where
  pPrint EmptyT = text "emptyT"
  pPrint (RTrail w) = text "r(" <> pPrint w <> text ")"
  pPrint (TTrail e1 e2) = pPrint e1 <> text ";" <> pPrint e2
  pPrint (BaTrail s t w1 w2) =
    vcat [ text "ba("
         , nest 2 (text s <> text ":" <> pPrint t <+> text "." <> pPrint w1 <> text ",")
         , nest 2 (pPrint w2)
         , text ")"]
  pPrint (BbTrail u t w1 w2) =
    vcat [ text "bb("
         , nest 2 (text u <> text ":" <> pPrint t <+> text "." <> pPrint w1 <> text ",")
         , nest 2 (pPrint w2)
         , text ")"]
  pPrint (TiTrail trail rWit tWit baWit bbWit tiWit lamWit appWit letWit trplWit) =
    text "ti("
      <> pPrint rWit <> text ","
      <+> pPrint tWit <> text ","
      <+> pPrint baWit <> text ","
      <+> pPrint bbWit <> text ","
      <+> pPrint tiWit <> text ","
      <+> pPrint lamWit <> text ","
      <+> pPrint appWit <> text ","
      <+> pPrint letWit <> text ","
      <+> pPrint trplWit <> text ")"
  pPrint (LamTrail s t e) =
    vcat [ text "lam("
         , nest 2 (text s <> text ":" <> pPrint t <+> text "." <> pPrint e)
         , text ")"]
  pPrint (AppTrail e1 e2) =
    vcat [ text "app("
         , nest 2 (pPrint e1 <> text ",")
         , nest 2 (pPrint e2)
         , text ")"]
  pPrint (LetTrail u t e1 e2) =
    vcat [ text "let("
         , nest 2 (text u <> text ":" <> pPrint t <+> text "." <> pPrint e1 <> text ",")
         , nest 2 (pPrint e2)
         , text ")"]
  pPrint (TrplTrail rTrl tTrl baTrl bbTrl tiTrl lamTrl appTrl letTrl trplTrl) =
    text "trpl("
      <> pPrint rTrl <> text ","
      <+> pPrint tTrl <> text ","
      <+> pPrint baTrl <> text ","
      <+> pPrint bbTrl <> text ","
      <+> pPrint tiTrl <> text ","
      <+> pPrint lamTrl <> text ","
      <+> pPrint appTrl <> text ","
      <+> pPrint letTrl <> text ","
      <+> pPrint trplTrl <> text ")"


newtype Program = Program Exp
  deriving Show

data Exp
  = Var String
  | Number Int
  | Boolean Bool
  | Brack Exp
  | Lam String Type Exp
  | App Exp Exp
  | AVar String
  | Bang Exp Trail
  | Let String Type Exp Exp
  | Inspect Exp Exp Exp Exp Exp Exp Exp Exp Exp
  deriving (Eq, Show)

data Value
  = IntVal Int
  | BoolVal Bool
  | ArrowVal InterpretEnv String Type Exp
  | BoxVal Witness Value
  deriving (Eq, Show)

instance Pretty Value where
  pPrint (IntVal i) = int i
  pPrint (BoolVal b) = text $ show b
  pPrint (ArrowVal _ arg typ body) = text "(" <> text arg <> text ":" <> pPrint typ <+> text "->" <+> text (show body) <+> text ")"
  pPrint (BoxVal witness value) =
    text "[" <+> pPrint witness <> text "]" <+> pPrint value

type ValuePair = (Value, Trail)

type InterpretEnv = (Env Value, Env Value)
