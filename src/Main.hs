module Main(main) where

import Parser( parseProgram )
import System.Environment ( getArgs )
import Typechecker
import qualified Env

main :: IO ()
main = do
  args <- getArgs
  result <- case args of
              []  -> fmap (parseProgram "<stdin>") getContents
              [f] -> fmap (parseProgram f) (readFile f)
              _   -> error "expected max. 1 argument"
  either putStrLn print result
