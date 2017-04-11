module Main(main) where

import Options.Applicative
import Data.Function((&))
import Parser( parseProgram )
import System.Environment ( getArgs )
import Typechecker
import Types
import qualified Env
import PreludeExtensions

data Mode = Parse
          | Typecheck
          | Evaluate
  deriving (Read)

data Args = Args
  { mode :: Mode
  , fileName :: String }

parseMode :: Parser Mode
parseMode = option auto
            ( long "mode"
            <> short 'm'
            <> metavar "MODE"
            <> value Typecheck )

parseArgs :: Parser Args
parseArgs = Args
         <$> parseMode
         <*> argument str (metavar "FILE" <> value "<stdin>")

opts :: ParserInfo Args
opts = info (parseArgs <**> helper)
  ( fullDesc
 <> progDesc "Interpreter for Audited Computation Language"
 <> header "Audited Computation Language Interpreter" )

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
        let typecheckResult = parseResult & bindRight typecheckProgramEmptyEnvs in
        case typecheckResult of
          (Left l) -> print l
          (Right (t, p)) -> ("Type: " ++ pretty t ++ "\nProof: " ++ pretty p) & putStrLn)
    Evaluate ->
      putStrLn "Evaluate not implemented"
