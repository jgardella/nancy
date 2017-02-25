module Language where

import qualified Types as T

data Program = Program Exp
  deriving Show

data Exp
  = Id String
  | Number Int
  | Boolean Bool
  | Brack Exp
  | Abs String T.Type Exp
  | App Exp Exp
  | AuditedVar String String String
  | AuditedUnit String Exp
  | AuditedComp String Exp Exp
  | TrailInspect String TrailMap TrailMap TrailMap TrailMap TrailMap TrailMap TrailMap TrailMap TrailMap TrailMap
  | DerivedTerm T.Trail Exp
  deriving Show

data TrailMap
  = Reflexivity String Exp
  | Symmetry String Exp
  | Transitivity String String Exp
  | Beta String String Exp
  | BetaBox String String Exp
  | TrailInspection String String Exp
  | Abstraction String Exp
  | Application String String Exp
  | Let String String Exp
  | Replacement String String String String String String String String String String Exp
  deriving Show
