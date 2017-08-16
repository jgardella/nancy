module AudiComp.Core.Errors.Interpreter where

import AudiComp.Core.Language
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

data InterpreterE
  = TruthVarUndefined String

instance Pretty InterpreterE where
  pPrint (TruthVarUndefined tVar) =
    text "Truth variable" <+> text tVar <+> text "is not defined"
