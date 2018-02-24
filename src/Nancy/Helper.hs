module Nancy.Helper where

import Nancy.Parser( parseProgram )
import Nancy.Typechecker
import Nancy.Interpreter
import Nancy.Core.Env as Env
import Nancy.Core.Language
import Nancy.Core.Util
import Nancy.Core.Output

parse :: FilePath -> String -> NancyOutput
parse source input =
  case parseProgram source input of
    (Right (Program (Bang body trail))) ->
      ParserOutput (Program (Bang body trail))
    (Right (Program nonBangExp)) ->
      ParserOutput (Program (Bang nonBangExp (RTrail (getWit nonBangExp))))
    (Left err) ->
      ParserError err

parseTypecheck :: FilePath -> String -> NancyOutput
parseTypecheck source input =
  parseTypecheckWithEnv source input (Env.empty, Env.empty)

parseTypecheckWithEnv :: FilePath -> String -> TypecheckEnv -> NancyOutput
parseTypecheckWithEnv source input env =
  case parse source input of
    (ParserOutput parseResult) ->
      case typecheckProgram env parseResult of
        (Right typePair) ->
          TypecheckOutput typePair
        (Left err) ->
          TypecheckError err
    err -> err

parseTypecheckInterpret :: FilePath -> String -> NancyOutput
parseTypecheckInterpret source input =
  case parse source input of
    ParserOutput parseResult ->
      case typecheckProgram (Env.empty, Env.empty) parseResult of
        Right _ ->
          case interpretProgram parseResult of
            (Right interpretResult, logs) ->
              InterpretOutput (interpretResult, logs)
            (Left err, logs) ->
              InterpretError (err, logs)
        Left err ->
          TypecheckError err
    err -> err
