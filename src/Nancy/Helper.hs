module Nancy.Helper where

import Nancy.Parser( parseProgram )
import Nancy.Typechecker
import Nancy.Interpreter
import Nancy.Core.Env as Env
import Nancy.Core.Language
import Nancy.Core.Util
import Nancy.Core.Errors
import Nancy.Core.Errors.Typechecker

parse :: FilePath -> String -> Either NancyError Program
parse source input =
  case parseProgram source input of
    (Right (Program (Bang body trail))) -> Right (Program (Bang body trail))
    (Right (Program nonBangExp)) -> Right (Program (Bang nonBangExp (RTrail (getWit nonBangExp))))
    (Left e) -> Left $ ParserErr e

parseAndTypecheck :: FilePath -> String -> Either NancyError TypePair
parseAndTypecheck source input =
  parseAndTypecheckWithEnv source input (Env.empty, Env.empty)

parseAndTypecheckWithEnv :: FilePath -> String -> TypecheckEnv -> Either NancyError TypePair
parseAndTypecheckWithEnv source input env = do
  parseResult <- parse source input
  typecheckProgram env parseResult

parseAndInterpret :: FilePath -> String -> (Either NancyError Value, [String])
parseAndInterpret source input =
  case parse source input of
    Right parseResult ->
      interpretProgram parseResult
    Left err -> (Left err, [])
