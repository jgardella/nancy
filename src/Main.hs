import Parser
import Lexer
import Typechecker
import qualified Context.Truth as CT

lexAndParse = calc . lexer

typecheckStr str = (typecheckProgram . lexAndParse) str CT.empty
