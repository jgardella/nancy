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

parseAndTypecheck :: FilePath -> String -> Either AudiCompError TypePair
parseAndTypecheck source input = do
  parseResult <- parse source input
  typecheckProgramEmptyEnvs parseResult

parseAndInterpret :: FilePath -> String -> Either AudiCompError ValuePair
parseAndInterpret source input = do
  parseResult <- parse source input
  interpretProgramEmptyEnvs parseResult
