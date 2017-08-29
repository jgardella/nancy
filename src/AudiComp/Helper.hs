module AudiComp.Helper where

import AudiComp.Parser( parseProgram )
import AudiComp.Typechecker
import AudiComp.Interpreter
import AudiComp.Core.Env as Env
import AudiComp.Core.Language
import AudiComp.Core.Errors
import AudiComp.Core.Errors.Typechecker

parse :: FilePath -> String -> Either AudiCompError Program
parse source input =
  case parseProgram source input of
    (Right x) -> Right x
    (Left e) -> Left $ ParserErr e

parseAndTypecheck :: FilePath -> String -> Either AudiCompError TypePair
parseAndTypecheck source input =
  parseAndTypecheckWithEnv source input (Env.empty, Env.empty, Env.empty)

parseAndTypecheckWithEnv :: FilePath -> String -> TypecheckEnv -> Either AudiCompError TypePair
parseAndTypecheckWithEnv source input env = do
  parseResult <- parse source input
  typecheckProgram env parseResult

parseAndInterpret :: FilePath -> String -> Either AudiCompError ValuePair
parseAndInterpret source input =
  parseAndInterpretWithEnv source input (Env.empty, Env.empty, Env.empty)

parseAndInterpretWithEnv :: FilePath -> String -> InterpretEnv -> Either AudiCompError ValuePair
parseAndInterpretWithEnv source input env = do
  parseResult <- parse source input
  interpretProgram env parseResult
