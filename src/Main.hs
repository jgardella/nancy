import Parser
import Lexer
import Typechecker
import System.Environment
import qualified Context.Truth as CT

lexAndParse = calc . lexer

typecheckStr str = (typecheckProgram . lexAndParse) str CT.empty

main :: IO ()
main = getArgs >>= print . typecheckStr . head
