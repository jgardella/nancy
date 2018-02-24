module Nancy.Core.Output where

import Text.PrettyPrint.HughesPJClass

import Nancy.Core.Language
import Nancy.Core.Errors.Typechecker
import Nancy.Core.Errors.Interpreter

data NancyOutput
  = ParserOutput Program
  | ParserError String
  | TypecheckOutput TypePair
  | TypecheckError TypecheckError
  | InterpretOutput (Value, [String])
  | InterpretError (InterpretError, [String])
  deriving (Eq, Show)

instance Pretty NancyOutput where
  pPrint (ParserOutput (Program expr)) =
    text (show expr)
  pPrint (ParserError err) =
    text "Error during lexing/parsing:" <+> text err
  pPrint (TypecheckOutput (resultType, resultWit)) =
    text "Type:\n" <> pPrint resultType <> text "\nWitness:\n" <> pPrint resultWit
  pPrint (TypecheckError err) =
    pPrint "Error during typechecking:" <+> pPrint err
  pPrint (InterpretOutput (resultVal, _)) =
    text "Value:\n" <> pPrint resultVal
  pPrint (InterpretError (err, _)) =
    pPrint "Error during interpreting:" <+> pPrint err
