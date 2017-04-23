module AudiComp.Core.Errors where

import AudiComp.Core.Language
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

data TypecheckerE
  = TruthVarUndefined String
  | InvalidArgType Type Type
  | ExpectedArrow Exp Type
  | ValidityVarUndefined String
  | ValidityVarWrongType String Type
  | ExpectedBox Type
  | TrailVarUndefined String
  | InconsistentTrailMappings

instance Pretty TypecheckerE where
  pPrint (TruthVarUndefined tVar) =
    text "Truth variable" <+> text tVar <+> text "is not defined"
  pPrint (InvalidArgType givenType expectedType) =
    text "Function expects type" <+> pPrint expectedType <> text ", but given type" <+> pPrint givenType
  pPrint (ExpectedArrow leftExp leftType) =
    text "Left expresion of App" <+> text (show leftExp) <+> text "has type" <+> pPrint leftType <>
      text ", should have type ArrowT"
  pPrint (ValidityVarUndefined vVar) =
    text "Validity variable" <+> text vVar <+> text "is not defined"
  pPrint (ValidityVarWrongType vVar vType) =
    text "Validity variable" <+> text vVar <+> text "has type" <+> pPrint vType <>
      text ", should have type AuditedT"
  pPrint (ExpectedBox beType) =
    text "Audited composition 'be' expression has type" <+> pPrint beType <>
      text ", should have type BoxT"
  pPrint (TrailVarUndefined tVar) =
    text "Trail variable" <+> text tVar <+> text "is not defined"
  pPrint InconsistentTrailMappings =
    text "All trail mappings should have same type"
