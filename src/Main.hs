module Main(main) where

import Options.Applicative
import Data.Function((&))
import Data.Semigroup((<>))
import AudiComp.Parser( parseProgram )
import System.Environment ( getArgs )
import AudiComp.Typechecker
import AudiComp.Interpreter
import qualified AudiComp.Core.Env as Env
import Text.PrettyPrint.HughesPJClass( Pretty, prettyShow )

data Mode = Parse
          | Typecheck
          | Interpret
  deriving (Read)

data Args = Args
  { mode :: Mode
  , pretty :: Bool
  , fileName :: String }

parseMode :: Parser Mode
parseMode = option auto
            ( long "mode"
            <> short 'm'
            <> metavar "MODE"
            <> value Typecheck )

parsePretty :: Parser Bool
parsePretty = switch
              ( long "pretty"
              <> short 'p' )

parseArgs :: Parser Args
parseArgs = Args
         <$> parseMode
         <*> parsePretty
         <*> argument str (metavar "FILE" <> value "<stdin>")

opts :: ParserInfo Args
opts = info (parseArgs <**> helper)
  ( fullDesc
 <> progDesc "Interpreter for Audited Computation Language"
 <> header "Audited Computation Language Interpreter" )

smartShow :: (Show a, Pretty a) => Args -> a -> String
smartShow args a =
  if pretty args then
    prettyShow a
  else
    show a

main :: IO ()
main = do
  args <- execParser opts
  let input = case fileName args of
                "<stdin>" -> getContents
                fileName -> readFile fileName
  case mode args of
    Parse -> do
      result <- fmap (parseProgram $ fileName args) input
      either putStrLn print result
    Typecheck ->
      input
      & fmap (parseProgram $ fileName args)
      >>= (\parseResult ->
        let typecheckResult = parseResult >>= typecheckProgramEmptyEnvs in
          case typecheckResult of
            (Left l) -> ("Error: " ++ show l) & putStrLn
            (Right (t, p)) -> ("Type: \n" ++ smartShow args t ++ "\nProof: \n" ++ smartShow args p) & putStrLn)
    Interpret ->
      input
      & fmap (parseProgram $ fileName args)
      >>= (\parseResult ->
        let interpretResult = parseResult >>= interpretProgramEmptyEnvs in
          case interpretResult of
            (Left l) -> ("Error: " ++ show l) & putStrLn
            (Right v) -> ("Value: \n" ++ smartShow args v) & putStrLn)
