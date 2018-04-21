module Nancy.Core.Errors.Interpreter where

import Nancy.Core.Language
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

data InterpretError
  = TruthVarUndefined String
  | ExpectedLam Value
  | ValidityVarUndefined String
  | ExpectedBang Value
  | BadTrailValue
  | InvalidTrailBranchList
  | InvalidPlusArgs Term
  | InvalidEqArgs Term
  | InvalidIfCond Term Value
  | PreInterpretError String
  deriving (Eq, Show)

instance Pretty InterpretError where
  pPrint (TruthVarUndefined tVar) =
    text "Truth variable" <+> text tVar <+> text "is not defined"
  pPrint (ExpectedLam value) =
    text "Expected Lam value, but got" <+> pPrint value
  pPrint (ValidityVarUndefined wVar) =
    text "Validity variable" <+> text wVar <+> text "is not defined"
  pPrint (ExpectedBang value) =
    text "Expected Bang value, but got" <+> pPrint value
  pPrint BadTrailValue =
    text "Bad trail value"
  pPrint (InvalidPlusArgs plusTerm) =
    text "Invalid plus args in expression:" <+> text (show plusTerm)
  pPrint (InvalidEqArgs eqTerm) =
    text "Invalid eq args in expression:" <+> text (show eqTerm)
  pPrint (InvalidIfCond condTerm condVal) =
    text "If condition should be boolean value, but expression" <+> text (show condTerm)
    <+> text "has value" <+> pPrint condVal
  pPrint InvalidTrailBranchList =
    text "Invalid trail branch list"
  pPrint (PreInterpretError err) =
    text err
