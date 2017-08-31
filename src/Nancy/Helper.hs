module Nancy.Helper where

import Nancy.Parser( parseProgram )
import Nancy.Typechecker
import Nancy.Interpreter
import Nancy.Core.Env as Env
import Nancy.Core.Language
import Nancy.Core.Errors
import Nancy.Core.Errors.Typechecker

parse :: FilePath -> String -> Either NancyError Program
parse source input =
  case parseProgram source input of
    (Right x) -> Right x
    (Left e) -> Left $ ParserErr e

parseAndTypecheck :: FilePath -> String -> Either NancyError TypePair
parseAndTypecheck source input =
  parseAndTypecheckWithEnv source input (Env.empty, Env.empty, Env.empty)

parseAndTypecheckWithEnv :: FilePath -> String -> TypecheckEnv -> Either NancyError TypePair
parseAndTypecheckWithEnv source input env = do
  parseResult <- parse source input
  typecheckProgram env parseResult

parseAndInterpret :: FilePath -> String -> Either NancyError ValuePair
parseAndInterpret source input =
  parseAndInterpretWithEnv source input (Env.empty, Env.empty, Env.empty)

parseAndInterpretWithEnv :: FilePath -> String -> InterpretEnv -> Either NancyError ValuePair
parseAndInterpretWithEnv source input env = do
  parseResult <- parse source input
  interpretProgram env parseResult
