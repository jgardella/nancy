module Nancy.Core.Errors.Interpreter where

import Nancy.Core.Language
import Nancy.Core.Env
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

data InterpretError
  = TruthVarUndefined String
  | TrailVarUndefined String (Env Trail)
  | ExpectedLam Value
  | ValidityVarUndefined String
  | ExpectedBang Value
  | BadTrailValue
  | PreInterpretError String
  deriving (Eq, Show)

instance Pretty InterpretError where
  pPrint (TruthVarUndefined tVar) =
    text "Truth variable" <+> text tVar <+> text "is not defined"
  pPrint (TrailVarUndefined eVar env) =
    text "Trail variable" <+> text eVar <+> text "is not defined in env: " <> pPrint env
  pPrint (ExpectedLam value) =
    text "Expected Lam value, but got" <+> pPrint value
  pPrint (ValidityVarUndefined wVar) =
    text "Validity variable" <+> text wVar <+> text "is not defined"
  pPrint (ExpectedBang value) =
    text "Expected Bang value, but got" <+> pPrint value
  pPrint BadTrailValue  =
    text "Bad trail value"
  pPrint (PreInterpretError err) =
    text err
