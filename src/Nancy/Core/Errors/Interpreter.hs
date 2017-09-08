module Nancy.Core.Errors.Interpreter where

import Nancy.Core.Language
import Nancy.Core.Env
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

data InterpreterE
  = TruthVarUndefined String
  | TrailVarUndefined String (Env Trail)
  | ExpectedArrow Value
  | ValidityVarUndefined String
  | ExpectedBox Value
  | InvalidTrailRename String
  deriving (Eq, Show)

instance Pretty InterpreterE where
  pPrint (TruthVarUndefined tVar) =
    text "Truth variable" <+> text tVar <+> text "is not defined"
  pPrint (TrailVarUndefined eVar env) =
    text "Trail variable" <+> text eVar <+> text "is not defined in env: " <> pPrint env
  pPrint (ExpectedArrow value) =
    text "Expected Arrow value, but got" <+> pPrint value
  pPrint (ValidityVarUndefined wVar) =
    text "Validity variable" <+> text wVar <+> text "is not defined"
  pPrint (ExpectedBox value) =
    text "Expected Box value"
  pPrint (InvalidTrailRename old) =
    text "Invalid trail rename, no existing trail variable '" <> text old <> text "'"
