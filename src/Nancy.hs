module Nancy where

import Nancy.Parser( parseProgram )
import Nancy.Typechecker
import Nancy.Interpreter
import Nancy.Core.Errors.Typechecker
import Nancy.Core.Errors.Interpreter
import Nancy.Core.Env as Env
import Nancy.Core.Language
import Nancy.Core.Util
import Nancy.Core.Output
import Text.PrettyPrint.HughesPJClass (prettyShow)

parse :: FilePath -> String -> ParserOutput
parse source input =
  case parseProgram source input of
    (Right (Program (Bang body trail))) ->
      ParseSuccess (Program (Bang body trail))
    (Right (Program nonBangExp)) ->
      ParseSuccess (Program (Bang nonBangExp (RTrail (getWit nonBangExp))))
    (Left err) ->
      ParseFailure err

parseTypecheck :: FilePath -> String -> TypecheckerOutput
parseTypecheck source input =
  parseTypecheckWithEnv source input (Env.empty, Env.empty)

parseTypecheckWithEnv :: FilePath -> String -> TypecheckEnv -> TypecheckerOutput
parseTypecheckWithEnv source input env =
  case parse source input of
    (ParseSuccess parseResult) ->
      typecheckProgram env parseResult
    (ParseFailure err) ->
      TypecheckFailure $ PreTypecheckError err

parseTypecheckInterpret :: Bool -> FilePath -> String -> InterpreterOutput
parseTypecheckInterpret prose source input =
  case parse source input of
    (ParseSuccess parseResult) ->
      case typecheckProgram (Env.empty, Env.empty) parseResult of
        (TypecheckSuccess _) ->
          case interpretProgram parseResult of
            (InterpretSuccess (value, logs)) ->
              let trailMode = if prose then Prose else Standard in
                InterpretSuccess (setValueTrailMode trailMode value, logs)
            (InterpretFailure errs) -> InterpretFailure errs
        (TypecheckFailure err) ->
          InterpretFailure (PreInterpretError (prettyShow err), [])
    (ParseFailure err) ->
      InterpretFailure (PreInterpretError err, [])
