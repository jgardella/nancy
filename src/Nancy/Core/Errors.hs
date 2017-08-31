module Nancy.Core.Errors where

import Text.PrettyPrint.HughesPJClass

import Nancy.Core.Errors.Typechecker
import Nancy.Core.Errors.Interpreter

data NancyError
  = ParserErr String
  | TypecheckErr TypecheckerE
  | InterpretErr InterpreterE
  deriving (Eq, Show)

instance Pretty NancyError where
  pPrint (ParserErr err) = text "Error during lexing/parsing:" <+> text err
  pPrint (TypecheckErr err) = text "Error during typechecking:" <+> pPrint err
  pPrint (InterpretErr err) = text "Error during interpreting:" <+> pPrint err
