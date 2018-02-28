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
  | InvalidTrailBranchList
  | InvalidPlusArgs Expr
  | InvalidEqArgs Expr
  | InvalidIfCond Expr Type
  | InvalidIfBranches Expr Type Expr Type
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
  pPrint InvalidTrailBranchList =
    text "Invalid trail branch list"
  pPrint (InvalidPlusArgs plusExpr) =
    text "Invalid plus args in expression:" <+> text (show plusExpr)
  pPrint (InvalidEqArgs eqExpr) =
    text "Invalid eq args in expression:" <+> text (show eqExpr)
  pPrint (InvalidIfCond condExpr condType) =
    text "If condition must have type boolean, but expression" <+> text (show condExpr)
    <+> text "has type" <+> pPrint condType
  pPrint (InvalidIfBranches thenExpr thenType elseExpr elseType) =
    text "If branches must have same type, but then branch has expression"
      <+> text (show thenExpr) <+> text "with type" <+> pPrint thenType
      <+> text "and else branch has expression" <+> text (show elseExpr) <+> text "with type" <+> pPrint elseType
  pPrint (PreTypecheckError err) =
    text err
