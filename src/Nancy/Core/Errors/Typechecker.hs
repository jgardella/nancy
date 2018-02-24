module Nancy.Core.Errors.Typechecker where

import Nancy.Core.Language
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

data TypecheckError
  = TruthVarUndefined String
  | InvalidArgType Type Type
  | InvalidLetArgType Type Type
  | ExpectedLam Exp Type
  | ValidityVarUndefined String
  | ValidityVarWrongType String Type
  | ExpectedBang Type
  | TrailVarUndefined String
  | InconsistentTrailMappings
  | InvalidRenameDomain [String] [String]
  | InvalidRenameCodomain [String] [String]
  deriving (Eq, Show)

instance Pretty TypecheckError where
  pPrint (TruthVarUndefined tVar) =
    text "Truth variable" <+> text tVar <+> text "is not defined"
  pPrint (InvalidArgType givenType expectedType) =
    text "Function expects type" <+> pPrint expectedType <> text ", but given type" <+> pPrint givenType
  pPrint (InvalidLetArgType givenType expectedType) =
    text "Let expects type" <+> pPrint expectedType <> text ", but given type" <+> pPrint givenType
  pPrint (ExpectedLam leftExp leftType) =
    text "Left expresion of App" <+> text (show leftExp) <+> text "has type" <+> pPrint leftType <>
      text ", should have type LamType"
  pPrint (ValidityVarUndefined vVar) =
    text "Validity variable" <+> text vVar <+> text "is not defined"
  pPrint (ValidityVarWrongType vVar vType) =
    text "Validity variable" <+> text vVar <+> text "has type" <+> pPrint vType <>
      text ", should have type AuditedT"
  pPrint (ExpectedBang beType) =
    text "Let 'be' expression has type" <+> pPrint beType <>
      text ", should have type BangType"
  pPrint (TrailVarUndefined tVar) =
    text "Trail variable" <+> text tVar <+> text "is not defined"
  pPrint InconsistentTrailMappings =
    text "All trail mappings should have same type"
  pPrint (InvalidRenameDomain boxTrailVars domain) =
    vcat [ text "Domain of trail renaming should match box trail variables"
         , nest 2 (text "domain:" <+> pPrint domain)
         , nest 2 (text "boxTrailVars:" <+> pPrint boxTrailVars)]
  pPrint (InvalidRenameCodomain initialTrailVars codomain) =
    vcat [ text "Codomain of trail renaming should match initial trail variables"
         , nest 2 (text "codomain:" <+> pPrint codomain)
         , nest 2 (text "initialTrailVars:" <+> pPrint initialTrailVars)]
