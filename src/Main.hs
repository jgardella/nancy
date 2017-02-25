module Main(main) where

import Options.Applicative
import Parser( parseProgram )
import System.Environment ( getArgs )
import Typechecker
import qualified Env

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
    Typecheck -> do
      parseResult <- fmap (parseProgram $ fileName args) input
      either print (print . typecheckProgramEmptyEnvs) parseResult
    Evaluate ->
      putStrLn "Evaluate not implemented"
  --result <- case args of
  --            []  -> fmap (parseProgram "<stdin>") getContents
  --            [f] -> fmap (parseProgram f) (readFile f)
  --            _   -> error "expected max. 1 argument"
  --either putStrLn print result
