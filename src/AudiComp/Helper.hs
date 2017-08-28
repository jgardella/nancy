module AudiComp.Helper where

import AudiComp.Parser
import AudiComp.Typechecker
import AudiComp.Interpreter
import AudiComp.Core.Language

parseAndTypecheck :: FilePath -> String -> Either String TypePair
parseAndTypecheck source input = do
  parseResult <- parseProgram source input
  typecheckProgramEmptyEnvs parseResult

parseAndInterpret :: FilePath -> String -> Either String ValuePair
parseAndInterpret source input = do
  parseResult <- parseProgram source input
  interpretProgramEmptyEnvs parseResult
