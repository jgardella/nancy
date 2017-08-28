module AudiComp.Core.Errors where

import Text.PrettyPrint.HughesPJClass

import AudiComp.Core.Errors.Typechecker
import AudiComp.Core.Errors.Interpreter

data AudiCompError
  = ParserErr String
  | TypecheckErr TypecheckerE
  | InterpretErr InterpreterE
  deriving (Eq, Show)

instance Pretty AudiCompError where
  pPrint (ParserErr err) = text "Error during lexing/parsing:" <+> text err
  pPrint (TypecheckErr err) = text "Error during typechecking:" <+> pPrint err
  pPrint (InterpretErr err) = text "Error during interpreting:" <+> pPrint err
