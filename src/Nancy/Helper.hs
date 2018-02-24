module Nancy.Helper where

import Nancy.Parser( parseProgram )
import Nancy.Typechecker
import Nancy.Interpreter
import Nancy.Core.Env as Env
import Nancy.Core.Language
import Nancy.Core.Util
import Nancy.Core.Errors

parse :: FilePath -> String -> Either NancyError Program
parse source input =
  case parseProgram source input of
    (Right (Program (Bang body trail))) -> Right (Program (Bang body trail))
    (Right (Program nonBangExp)) -> Right (Program (Bang nonBangExp (RTrail (getWit nonBangExp))))
    (Left e) -> Left $ ParserErr e

parseTypecheck :: FilePath -> String -> Either NancyError TypePair
parseTypecheck source input =
  parseTypecheckWithEnv source input (Env.empty, Env.empty)

parseTypecheckWithEnv :: FilePath -> String -> TypecheckEnv -> Either NancyError TypePair
parseTypecheckWithEnv source input env = do
  parseResult <- parse source input
  typecheckProgram env parseResult

parseTypecheckInterpret :: FilePath -> String -> (Either NancyError Value, [String])
parseTypecheckInterpret source input =
  case parse source input of
    Right parseResult ->
      case typecheckProgram (Env.empty, Env.empty) parseResult of
        Right _ ->
          interpretProgram parseResult
        Left err -> (Left err, [])
    Left err -> (Left err, [])
