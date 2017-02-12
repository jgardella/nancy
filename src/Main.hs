import Parser
import Lexer
import Typechecker
import System.Environment
import qualified Env as Env

lexAndParse = calc . lexer

typecheckStr str = (typecheckProgram . lexAndParse) str Env.empty Env.empty Env.empty

main :: IO ()
main = getArgs >>= print . typecheckStr . head
