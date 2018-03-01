module Main(main) where

import Options.Applicative
import Data.Semigroup((<>))
import Control.Monad(unless)
import System.IO
import Nancy
import Text.PrettyPrint.HughesPJClass( Pretty, prettyShow )

data Mode = Parse
          | Typecheck
          | Interpret
  deriving (Read)

data Args = Args
  { mode :: Mode
  , printShow :: Bool
  , source :: String }

parseMode :: Parser Mode
parseMode = option auto
            ( long "mode"
            <> short 'm'
            <> metavar "MODE"
            <> value Interpret )

parseShow :: Parser Bool
parseShow = switch
              ( long "show"
              <> short 's' )

parseArgs :: Parser Args
parseArgs = Args
         <$> parseMode
         <*> parseShow
         <*> argument str (metavar "FILE" <> value "<stdin>")

opts :: ParserInfo Args
opts = info (parseArgs <**> helper)
  ( fullDesc
 <> progDesc "Interpreter for Nancy Programming Language"
 <> header "Nancy Interpreter" )

read' :: IO String
read' = putStr "#> "
     >> hFlush stdout
     >> getLine

evalPrint' :: Args -> String -> IO ()
evalPrint' args input =
  case mode args of
    Parse ->
      print' args $ parse (source args) input
    Typecheck ->
      print' args $ parseTypecheck (source args) input
    Interpret ->
      print' args $ parseTypecheckInterpret (source args) input

print' :: (Show a, Pretty a) => Args -> a -> IO ()
print' args output =
  if printShow args then
    print output
  else
    putStrLn $ prettyShow output

repl :: Args -> IO ()
repl args = do
  input <- read'

  unless (input == ":quit")
    $ evalPrint' args input
   >> repl args

main :: IO ()
main = do
  args <- execParser opts
  case source args of
    "<stdin>" -> repl args
    fileName -> do
      input <- readFile fileName
      evalPrint' args input
