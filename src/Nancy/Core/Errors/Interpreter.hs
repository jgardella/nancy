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
  | InvalidPlusArgs Expr
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
  pPrint (InvalidPlusArgs plusExpr) =
    text "Invalid plus args in expression:" <+> text (show plusExpr)
  pPrint InvalidTrailBranchList =
    text "Invalid trail branch list"
  pPrint (PreInterpretError err) =
    text err
