module Nancy.Core.Language where

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

data Type
  = IntType
  | BoolType
  | LamType Type Type
  | BangType Type Witness
  deriving (Eq, Show)

instance Pretty Type where
  pPrint IntType = text "int"
  pPrint BoolType = text "bool"
  pPrint (LamType argType returnType) = pPrint argType <+> text "->" <+> pPrint returnType
  pPrint (BangType bodyType witness) = pPrint bodyType <+> brackets(pPrint witness)

mapType :: (Witness -> Witness) -> (Type -> Type) -> Type -> Type
mapType _ typeFunc (LamType argType returnType) =
  LamType (typeFunc argType) (typeFunc returnType)
mapType witFunc typeFunc (BangType bangType bangWit) =
  BangType (typeFunc bangType) (witFunc bangWit)
mapType _ _ otherType = otherType

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

instance Pretty Witness where
  pPrint (VarWit var) = text "varwit" <> parens(text var)
  pPrint (IntWit intVal) = text "intwit" <> parens(text (show intVal))
  pPrint (BoolWit boolVal) = text "boolwit" <> parens(text (show boolVal))
  pPrint (LamWit arg argType bodyWit) = text "lamwit" <> parens(text arg <> colon <> pPrint argType <> comma <+> pPrint bodyWit)
  pPrint (AppWit lamWit argWit) = text "appwit" <> parens(pPrint lamWit <> comma <> pPrint argWit)
  pPrint (AVarWit avar) = text "avarwit" <> parens(text avar)
  pPrint (BangWit bangWit) = text "bangwit" <> parens(pPrint bangWit)
  pPrint (LetWit arg argType argWit bodyWit) =
    text "letwit" <> parens(text arg <> colon <> pPrint argType <> comma
      <+> pPrint argWit <> comma <+> pPrint bodyWit)
  pPrint (TiWit (TrailBranches rWit tWit baWit bbWit tiWit lamWit appWit letWit trplWit)) =
    text "tiwit" <> parens(
      pPrint rWit <> comma
      <+> pPrint tWit <> comma
      <+> pPrint baWit <> comma
      <+> pPrint bbWit <> comma
      <+> pPrint tiWit <> comma
      <+> pPrint lamWit <> comma
      <+> pPrint appWit <> comma
      <+> pPrint letWit <> comma
      <+> pPrint trplWit)

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

instance Pretty Trail where
  pPrint (RTrail wit) = text "r" <> parens(pPrint wit)
  pPrint (TTrail trail1 trail2) = pPrint trail1 <> semi <> pPrint trail2
  pPrint (BaTrail arg argType argWit bodyWit) =
    text "ba" <> parens(
    text arg <> colon <> pPrint argType <> comma <+> pPrint argWit <> comma
    <+> pPrint bodyWit)
  pPrint (BbTrail arg argType argWit bodyWit) =
    text "bb" <> parens(
    text arg <> colon <> pPrint argType <> comma <+> pPrint argWit <> comma
    <+> pPrint bodyWit)
  pPrint (TiTrail _ (TrailBranches rWit tWit baWit bbWit tiWit lamWit appWit letWit trplWit)) =
    text "ti" <> parens(
      pPrint rWit <> comma
      <+> pPrint tWit <> comma
      <+> pPrint baWit <> comma
      <+> pPrint bbWit <> comma
      <+> pPrint tiWit <> comma
      <+> pPrint lamWit <> comma
      <+> pPrint appWit <> comma
      <+> pPrint letWit <> comma
      <+> pPrint trplWit)
  pPrint (LamTrail arg argType bodyTrail) =
    text "lam" <> parens(
    text arg <> colon <> pPrint argType <> comma <+> pPrint bodyTrail)
  pPrint (AppTrail lamTrail argTrail) =
    text "app" <> parens(
    pPrint lamTrail <> comma
    <+> pPrint argTrail)
  pPrint (LetTrail arg argType argTrail bodyTrail) =
    text "let" <> parens(
    text arg <> colon <> pPrint argType <> comma <+> pPrint argTrail <> comma
    <+> pPrint bodyTrail)
  pPrint (TrplTrail (TrailBranches rTrl tTrl baTrl bbTrl tiTrl lamTrl appTrl letTrl trplTrl)) =
    text "trpl" <> parens(
      pPrint rTrl <> comma
      <+> pPrint tTrl <> comma
      <+> pPrint baTrl <> comma
      <+> pPrint bbTrl <> comma
      <+> pPrint tiTrl <> comma
      <+> pPrint lamTrl <> comma
      <+> pPrint appTrl <> comma
      <+> pPrint letTrl <> comma
      <+> pPrint trplTrl)

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

newtype Program = Program Expr
  deriving (Eq, Show)

data Expr
  = Var String
  | Number Int
  | Boolean Bool
  | Brack Expr
  | Lam String Type Expr
  | App Expr Expr
  | AVar String
  | Bang Expr Trail
  | Let String Type Expr Expr
  | Inspect (TrailBranches Expr)
  deriving (Eq, Show)

mapExpr :: (Expr -> Expr) -> Expr -> Expr
mapExpr f (Brack brackExpr) = f brackExpr
mapExpr f (Lam var varType bodyExpr) =
  Lam var varType (f bodyExpr)
mapExpr f (App lamExpr argExpr) =
  App (f lamExpr) (f argExpr)
mapExpr f (Bang bangExpr bangTrail) =
  Bang (f bangExpr) bangTrail
mapExpr f (Let var varType argExpr bodyExpr) =
  Let var varType (f argExpr) (f bodyExpr)
mapExpr f (Inspect branches) =
  Inspect $ fmap f branches
mapExpr _ otherExpr = otherExpr

data Value
  = IntVal Int
  | BoolVal Bool
  | LamVal String Type Expr
  | VarVal String
  | AVarVal String
  | BangVal Value Trail
  deriving (Eq, Show)

instance Pretty Value where
  pPrint (IntVal i) = int i
  pPrint (BoolVal b) = text $ show b
  pPrint (LamVal arg typ body) = parens(text arg <> colon <> pPrint typ <+> text "->" <+> text (show body))
  pPrint (VarVal x) = text x
  pPrint (AVarVal u) = text "<" <> text u <> text ">"
  pPrint (BangVal value trail) = text "!" <> brackets(pPrint trail) <+> pPrint value
