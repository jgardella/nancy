module Main(main) where

import Options.Applicative
import Data.Function((&))
import Data.Semigroup((<>))
import Control.Monad(unless)
import System.IO
import Nancy.Helper
import Text.PrettyPrint.HughesPJClass( Pretty, prettyShow )

data Mode = Parse
          | Typecheck
          | Interpret
  deriving (Read)

data Args = Args
  { mode :: Mode
  , pretty :: Bool
  , source :: String }

parseMode :: Parser Mode
parseMode = option auto
            ( long "mode"
            <> short 'm'
            <> metavar "MODE"
            <> value Interpret )

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
 <> progDesc "Interpreter for Nancy Programming Language"
 <> header "Nancy Interpreter" )

smartShow :: (Show a, Pretty a) => Args -> a -> String
smartShow args a =
  if pretty args then
    prettyShow a
  else
    show a

read' :: IO String
read' = putStr "#> "
     >> hFlush stdout
     >> getLine

eval :: Args -> String -> IO ()
eval args input =
  case mode args of
    Parse ->
      either (putStrLn . smartShow args) print (parse (source args) input)
    Typecheck ->
      case parseTypecheck (source args) input of
        (Left l) -> ("Error: " ++ smartShow args l) & putStrLn
        (Right (t, p)) -> ("Type: \n" ++ smartShow args t ++ "\nWitness: \n" ++ smartShow args p) & putStrLn
    Interpret -> do
      let (interpretResult, logs) = parseTypecheckInterpret (source args) input
      mapM_ putStrLn logs
      case interpretResult of
        (Left l) -> ("Error: " ++ smartShow args l) & putStrLn
        (Right v) -> ("Value: \n" ++ smartShow args v) & putStrLn

repl :: Args -> IO ()
repl args = do
  input <- read'

  unless (input == ":quit")
    $ eval args input >> repl args

main :: IO ()
main = do
  args <- execParser opts
  case source args of
    "<stdin>" -> repl args
    fileName -> do
      input <- readFile fileName
      eval args input
