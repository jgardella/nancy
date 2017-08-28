module AudiComp.Helper where

import AudiComp.Parser( parseProgram )
import AudiComp.Typechecker
import AudiComp.Interpreter
import AudiComp.Core.Language
import AudiComp.Core.Errors
import AudiComp.Core.Errors.Typechecker

parse :: FilePath -> String -> Either AudiCompError Program
parse source input =
  case parseProgram source input of
    (Right x) -> Right x
    (Left e) -> Left $ ParserErr e

parseAndTypecheck :: FilePath -> String -> Either String TypePair
parseAndTypecheck source input = do
  parseResult <- parseProgram source input
  typecheckProgramEmptyEnvs parseResult

parseAndInterpret :: FilePath -> String -> Either String ValuePair
parseAndInterpret source input = do
  parseResult <- parseProgram source input
  interpretProgramEmptyEnvs parseResult
