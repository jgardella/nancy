module Nancy.Core.Errors.Typechecker where

import Nancy.Core.Language
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

data TypecheckError
  = TruthVarUndefined String
  | InvalidArgType Type Type
  | InvalidLetArgType Type Type
  | ExpectedLam Expr Type
  | ValidityVarUndefined String
  | ExpectedBang Type
  | BadInspectBranch Expr
  | PreTypecheckError String
  deriving (Eq, Show)

instance Pretty TypecheckError where
  pPrint (TruthVarUndefined tVar) =
    text "Truth variable" <+> text tVar <+> text "is not defined"
  pPrint (InvalidArgType givenType expectedType) =
    text "Function expects type" <+> pPrint expectedType <> text ", but given type" <+> pPrint givenType
  pPrint (InvalidLetArgType givenType expectedType) =
    text "Let expects type" <+> pPrint expectedType <> text ", but given type" <+> pPrint givenType
  pPrint (ExpectedLam leftExpr leftType) =
    text "Left expresion of App" <+> text (show leftExpr) <+> text "has type" <+> pPrint leftType <>
      text ", but should have type LamType"
  pPrint (ValidityVarUndefined vVar) =
    text "Validity variable" <+> text vVar <+> text "is not defined"
  pPrint (ExpectedBang beType) =
    text "Let 'be' expression has type" <+> pPrint beType <>
      text ", should have type BangType"
  pPrint (BadInspectBranch inspectExpr) =
    text "Inspect expression has at least one badly-typed branch:" <+> text (show inspectExpr)
  pPrint (PreTypecheckError err) =
    text err
