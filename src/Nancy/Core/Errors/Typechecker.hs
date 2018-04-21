module Nancy.Core.Errors.Typechecker where

import Nancy.Core.Language
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

data TypecheckError
  = TruthVarUndefined String
  | InvalidArgType Type Type
  | InvalidLetArgType Type Type
  | ExpectedLam Term Type
  | ValidityVarUndefined String
  | ExpectedBang Type
  | BadInspectBranch Term
  | InvalidTrailBranchList
  | InvalidPlusArgs Term
  | InvalidEqArgs Term
  | InvalidIfCond Term Type
  | InvalidIfBranches Term Type Term Type
  | PreTypecheckError String
  deriving (Eq, Show)

instance Pretty TypecheckError where
  pPrint (TruthVarUndefined tVar) =
    text "Truth variable" <+> text tVar <+> text "is not defined"
  pPrint (InvalidArgType givenType expectedType) =
    text "Function expects type" <+> pPrint expectedType <> text ", but given type" <+> pPrint givenType
  pPrint (InvalidLetArgType givenType expectedType) =
    text "Let expects type" <+> pPrint expectedType <> text ", but given type" <+> pPrint givenType
  pPrint (ExpectedLam leftTerm leftType) =
    text "Left expresion of App" <+> text (show leftTerm) <+> text "has type" <+> pPrint leftType <>
      text ", but should have type LamType"
  pPrint (ValidityVarUndefined vVar) =
    text "Validity variable" <+> text vVar <+> text "is not defined"
  pPrint (ExpectedBang beType) =
    text "Let 'be' expression has type" <+> pPrint beType <>
      text ", should have type BangType"
  pPrint (BadInspectBranch inspectTerm) =
    text "Inspect expression has at least one badly-typed branch:" <+> text (show inspectTerm)
  pPrint InvalidTrailBranchList =
    text "Invalid trail branch list"
  pPrint (InvalidPlusArgs plusTerm) =
    text "Invalid plus args in expression:" <+> text (show plusTerm)
  pPrint (InvalidEqArgs eqTerm) =
    text "Invalid eq args in expression:" <+> text (show eqTerm)
  pPrint (InvalidIfCond condTerm condType) =
    text "If condition must have type boolean, but expression" <+> text (show condTerm)
    <+> text "has type" <+> pPrint condType
  pPrint (InvalidIfBranches thenTerm thenType elseTerm elseType) =
    text "If branches must have same type, but then branch has expression"
      <+> text (show thenTerm) <+> text "with type" <+> pPrint thenType
      <+> text "and else branch has expression" <+> text (show elseTerm) <+> text "with type" <+> pPrint elseType
  pPrint (PreTypecheckError err) =
    text err
