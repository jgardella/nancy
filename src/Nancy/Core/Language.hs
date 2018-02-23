module Nancy.Core.Language where

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

data Type
  = IntType
  | BoolType
  | LamType Type Type
  | BangType Type Witness
  deriving (Eq, Show)

mapType :: (Witness -> Witness) -> (Type -> Type) -> Type -> Type
mapType _ typeFunc (LamType argType returnType) =
  LamType (typeFunc argType) (typeFunc returnType)
mapType witFunc typeFunc (BangType bangType bangWit) =
  BangType (typeFunc bangType) (witFunc bangWit)
mapType _ _ otherType = otherType

instance Pretty Type where
  pPrint IntType = text "int"
  pPrint BoolType = text "bool"
  pPrint (LamType argType returnType) = pPrint argType <+> text "->" <+> pPrint returnType
  pPrint (BangType bodyType witness) = text "[" <> pPrint witness <> text "]" <+> pPrint bodyType

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

mapWitness :: (Type -> Type) -> (Witness -> Witness) -> Witness -> Witness
mapWitness typeFunc witFunc (LamWit var varType lamWit) =
  LamWit var (typeFunc varType) (witFunc lamWit)
mapWitness _ witFunc (AppWit lamWit argWit) =
  AppWit (witFunc lamWit) (witFunc argWit)
mapWitness _ witFunc (BangWit bangWit) =
  BangWit $ witFunc bangWit
mapWitness typeFunc witFunc (LetWit var varType argWit bodyWit) =
  LetWit var (typeFunc varType) (witFunc argWit) (witFunc bodyWit)
mapWitness typeFunc witFunc (TiWit branchWits) =
  TiWit $ fmap (mapWitness typeFunc witFunc) branchWits
mapWitness _ _ otherWitness = otherWitness

instance Pretty Witness where
  pPrint (VarWit var) = text "varwit(" <> text var <> text ")"
  pPrint (IntWit intVal) = text "intwit(" <> text (show intVal) <> text ")"
  pPrint (BoolWit boolVal) = text "boolwit(" <> text (show boolVal) <> text ")"
  pPrint (LamWit arg argType bodyWit) = text "lamwit(" <> text arg <> text ":" <> pPrint argType <> text "," <+> pPrint bodyWit <> text ")"
  pPrint (AppWit lamWit argWit) = text "appwit(" <> pPrint lamWit <> text ", " <> pPrint argWit <> text ")"
  pPrint (AVarWit avar) = text "avarwit(" <> text avar <> text ")"
  pPrint (BangWit bangWit) = text "bangwit(" <> pPrint bangWit <> text")"
  pPrint (LetWit arg argType argWit bodyWit) =
    text "letwit(" <> text arg <> text ":" <> pPrint argType <> text ","
      <+> pPrint argWit <> text "," <+> pPrint bodyWit <> text ")"
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

data TrailBranches a = TrailBranches {
  rB :: a,
  tB :: a,
  baB :: a,
  bbB :: a,
  tiB :: a,
  lamB :: a,
  appB :: a,
  letB :: a,
  trplB :: a
} deriving (Eq, Show)

instance Functor TrailBranches where
  fmap f TrailBranches{
    rB=rVal ,
    tB=tVal,
    baB=baVal,
    bbB=bbVal,
    tiB=tiVal,
    lamB=lamVal,
    appB=appVal,
    letB=letVal,
    trplB=trplVal
  } =
    TrailBranches{
      rB=f rVal,
      tB=f tVal,
      baB=f baVal,
      bbB=f bbVal,
      tiB=f tiVal,
      lamB=f lamVal,
      appB=f appVal,
      letB=f letVal,
      trplB=f trplVal
    }

trailBranchesToList :: TrailBranches a -> [a]
trailBranchesToList TrailBranches{
  rB=rVal ,
  tB=tVal,
  baB=baVal,
  bbB=bbVal,
  tiB=tiVal,
  lamB=lamVal,
  appB=appVal,
  letB=letVal,
  trplB=trplVal
} = [rVal, tVal, baVal, bbVal, tiVal, lamVal, appVal, letVal, trplVal]

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
mapTrail _ trailFunc _ (TTrail trail1 trail2) =
  TTrail (trailFunc trail1) (trailFunc trail2)
mapTrail witFunc _ typeFunc (BaTrail var varType argWit bodyWit) =
  BaTrail var (typeFunc varType) (witFunc argWit) (witFunc bodyWit)
mapTrail witFunc _ typeFunc (BbTrail var varType argWit bodyWit) =
  BbTrail var (typeFunc varType) (witFunc argWit) (witFunc bodyWit)
mapTrail witFunc trailFunc _ (TiTrail trail branchWits) =
  TiTrail (trailFunc trail) (fmap witFunc branchWits)
mapTrail _ trailFunc typeFunc (LamTrail var varType lamTrail) =
  LamTrail var (typeFunc varType) (trailFunc lamTrail)
mapTrail _ trailFunc _ (AppTrail lamTrail argTrail) =
  AppTrail (trailFunc lamTrail) (trailFunc argTrail)
mapTrail _ trailFunc typeFunc (LetTrail var varType argTrail bodyTrail) =
  LetTrail var (typeFunc varType) (trailFunc argTrail) (trailFunc bodyTrail)
mapTrail _ trailFunc _ (TrplTrail branchTrails) =
  TrplTrail $ fmap trailFunc branchTrails

(<-->) :: Trail -> Trail -> Trail
trailOne <--> trailTwo = TTrail trailOne trailTwo

infixr 0 <-->

instance Pretty Trail where
  pPrint (RTrail wit) = text "r(" <> pPrint wit <> text ")"
  pPrint (TTrail trail1 trail2) = pPrint trail1 <> text ";" <> pPrint trail2
  pPrint (BaTrail arg argType argWit bodyWit) =
    text "ba("
    <> text arg <> text ":" <> pPrint argType <> text "," <+> pPrint argWit <> text ","
    <+> pPrint bodyWit
    <> text ")"
  pPrint (BbTrail arg argType argWit bodyWit) =
    text "bb("
    <> text arg <> text ":" <> pPrint argType <> text "," <+> pPrint argWit <> text ","
    <+> pPrint bodyWit
    <> text ")"
  pPrint (TiTrail _ (TrailBranches rWit tWit baWit bbWit tiWit lamWit appWit letWit trplWit)) =
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
  pPrint (LamTrail arg argType bodyTrail) =
    text "lam("
    <> text arg <> text ":" <> pPrint argType <> text "," <+> pPrint bodyTrail
    <> text ")"
  pPrint (AppTrail lamTrail argTrail) =
    text "app("
    <> pPrint lamTrail <> text ","
    <+> pPrint argTrail
    <> text ")"
  pPrint (LetTrail arg argType argTrail bodyTrail) =
    text "let("
    <> text arg <> text ":" <> pPrint argType <> text "," <+> pPrint argTrail <> text ","
    <+> pPrint bodyTrail
    <> text ")"
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

mapExp :: (Exp -> Exp) -> Exp -> Exp
mapExp f (Brack brackExp) = f brackExp
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
mapExp _ otherExp = otherExp

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
