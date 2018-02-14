module Nancy.Core.Language where

import Nancy.Core.Env
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

data Type
  = IntType
  | BoolType
  | LamType Type Type
  | BangType Type Witness
  deriving (Eq, Show)

mapType _ typeFunc (LamType argType returnType) =
  LamType (typeFunc argType) (typeFunc returnType)
mapType witFunc typeFunc (BangType bangType bangWit) =
  BangType (typeFunc bangType) (witFunc bangWit)
mapType witFunc typeFunc otherType = otherType

instance Pretty Type where
  pPrint IntType = text "int"
  pPrint BoolType = text "bool"
  pPrint (LamType l r) = pPrint l <+> text "->" <+> pPrint r
  pPrint (BangType t w) = text "[" <> pPrint w <> text "]" <+> pPrint t

data Witness
  = VarWit String
  | IntWit Int
  | BoolWit Bool
  | LamWit String Type Witness
  | AppWit Witness Witness
  | AVarWit String
  | BangWit Witness
  | LetWit String Type Witness Witness
  | TiWit (TrailBranches Witness)
  deriving (Eq, Show)

mapWitness typeFunc witFunc (LamWit var varType lamWit) =
  LamWit var (typeFunc varType) (witFunc lamWit)
mapWitness typeFunc witFunc (AppWit lamWit argWit) =
  AppWit (witFunc lamWit) (witFunc argWit)
mapWitness typeFunc witFunc (BangWit bangWit) =
  BangWit $ witFunc bangWit
mapWitness typeFunc witFunc (LetWit var varType argWit bodyWit) =
  LetWit var (typeFunc varType) (witFunc argWit) (witFunc bodyWit)
mapWitness typeFunc witFunc (TiWit branchWits) =
  TiWit $ fmap (mapWitness typeFunc witFunc) branchWits
mapWitness _ _ otherWitness = otherWitness

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
  pPrint (TiWit (TrailBranches rWit tWit baWit bbWit tiWit lamWit appWit letWit trplWit)) =
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

data TrailBranches a = TrailBranches a a a a a a a a a
  deriving (Eq, Show)

instance Functor TrailBranches where
  fmap f (TrailBranches rB tB baB bbB tiB lamB appB letB trplB) =
    TrailBranches
      (f rB)
      (f tB)
      (f baB)
      (f bbB)
      (f tiB)
      (f lamB)
      (f appB)
      (f letB)
      (f trplB)

data Trail
    = RTrail Witness
    | TTrail Trail Trail
    | BaTrail String Type Witness Witness
    | BbTrail String Type Witness Witness
    | TiTrail Trail (TrailBranches Witness)
    | LamTrail String Type Trail
    | AppTrail Trail Trail
    | LetTrail String Type Trail Trail
    | TrplTrail (TrailBranches Trail)
    deriving (Eq, Show)

mapTrail :: (Witness -> Witness) -> (Trail -> Trail) -> (Type -> Type) -> Trail -> Trail
mapTrail witFunc _ _ (RTrail rWit) =
  RTrail $ witFunc rWit
mapTrail witFunc trailFunc _ (TTrail trail1 trail2) =
  TTrail (trailFunc trail1) (trailFunc trail2)
mapTrail witFunc trailFunc typeFunc (BaTrail var varType argWit bodyWit) =
  BaTrail var (typeFunc varType) (witFunc argWit) (witFunc bodyWit)
mapTrail witFunc trailFunc typeFunc (BbTrail var varType argWit bodyWit) =
  BbTrail var (typeFunc varType) (witFunc argWit) (witFunc bodyWit)
mapTrail witFunc trailFunc _ (TiTrail trail branchWits) =
  TiTrail (trailFunc trail) (fmap witFunc branchWits)
mapTrail witFunc trailFunc typeFunc (LamTrail var varType lamTrail) =
  LamTrail var (typeFunc varType) (trailFunc lamTrail)
mapTrail witFunc trailFunc _ (AppTrail lamTrail argTrail) =
  AppTrail (trailFunc lamTrail) (trailFunc argTrail)
mapTrail witFunc trailFunc typeFunc (LetTrail var varType argTrail bodyTrail) =
  LetTrail var (typeFunc varType) (trailFunc argTrail) (trailFunc bodyTrail)
mapTrail witFunc trailFunc _ (TrplTrail branchTrails) =
  TrplTrail $ fmap trailFunc branchTrails

(<-->) :: Trail -> Trail -> Trail
trailOne <--> trailTwo = TTrail trailOne trailTwo

infixl 0 <-->

instance Pretty Trail where
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
  pPrint (TiTrail trail (TrailBranches rWit tWit baWit bbWit tiWit lamWit appWit letWit trplWit)) =
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
  pPrint (TrplTrail (TrailBranches rTrl tTrl baTrl bbTrl tiTrl lamTrl appTrl letTrl trplTrl)) =
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
  | Inspect (TrailBranches Exp)
  deriving (Eq, Show)

mapExp f (Brack exp) = f exp
mapExp f (Lam var varType bodyExp) =
  Lam var varType (f bodyExp)
mapExp f (App lamExp argExp) =
  App (f lamExp) (f argExp)
mapExp f (Bang bangExp bangTrail) =
  Bang (f bangExp) bangTrail
mapExp f (Let var varType argExp bodyExp) =
  Let var varType (f argExp) (f bodyExp)
mapExp f (Inspect branches) =
  Inspect $ fmap f branches
mapExp f otherExp = otherExp

data Value
  = IntVal Int
  | BoolVal Bool
  | LamVal String Type Exp
  | VarVal String
  | AVarVal String
  | BangVal Value Trail
  deriving (Eq, Show)

instance Pretty Value where
  pPrint (IntVal i) = int i
  pPrint (BoolVal b) = text $ show b
  pPrint (LamVal arg typ body) = text "(" <> text arg <> text ":" <> pPrint typ <+> text "->" <+> text (show body) <+> text ")"
  pPrint (VarVal x) = text x
  pPrint (AVarVal u) = text "<" <> text u <> text ">"
  pPrint (BangVal value trail) = text "![" <> pPrint trail <> text "]" <+> pPrint value

type ValuePair = (Value, Trail)
