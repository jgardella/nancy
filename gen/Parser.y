{
{-# OPTIONS -w #-}
module Nancy.Parser( parseProgram ) where

import Data.Char
import Nancy.Lexer
import Nancy.Core.Language
import Nancy.Core.Errors
}

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TokenEOF }
-- Without this we get a type error
%error { happyError }


%token
  id    { Token _ (TokenVar $$) }
  num   { Token _ (TokenNum $$) }
  fun   { Token _ TokenFun }
  insp  { Token _ TokenInspect }
  '->'  { Token _ TokenArrow }
  '<'   { Token _ TokenLBrack }
  '>'   { Token _ TokenRBrack }
  '('   { Token _ TokenLParen }
  ')'   { Token _ TokenRParen }
  '['   { Token _ TokenLSquare }
  ']'   { Token _ TokenRSquare }
  '{'   { Token _ TokenLBrace }
  '}'   { Token _ TokenRBrace }
  ':'   { Token _ TokenColon }
  ';'   { Token _ TokenSemi }
  '!'   { Token _ TokenBang }
  '.'   { Token _ TokenDot }
  'r'   { Token _ TokenR }
  't'   { Token _ TokenT }
  ba    { Token _ TokenBA }
  bb    { Token _ TokenBB }
  ti    { Token _ TokenTI }
  lam   { Token _ TokenLAM }
  app   { Token _ TokenAPP }
  trpl  { Token _ TokenTRPL }
  let   { Token _ TokenLet }
  be    { Token _ TokenBe }
  in    { Token _ TokenIn }
  int   { Token _ TokenInt }
  bool  { Token _ TokenBool }
  true  { Token _ TokenTrue }
  false { Token _ TokenFalse }


%%

Program   : Exp                              { Program $1 }
Type      : int                              { IntT }
          | bool                             { BoolT }
          | Type '->' Type                   { ArrowT $1 $3 }
Exp       : id                               { Id $1 }
          | num                              { Number $1 }
          | true                             { Boolean True }
          | false                            { Boolean False }
          | '(' Exp ')'                      { Brack $2 }
          | fun '(' id ':' Type ')' '->' Exp { Abs $3 $5 $8 }
          | Exp Exp                          { App $1 $2 }
          | '<' id '>'                       { AuditedVar $2 }
          | '!' Exp                          { AuditedUnit $2 }
          | let id ':' Type be Exp in Exp    { AuditedComp $2 $4 $6 $8 }
          | insp '['
              'r' '->' Exp
              't' '->' Exp
              ba '->' Exp
              bb '->' Exp
              ti '->' Exp
              lam '->' Exp
              app '->' Exp
              let '->' Exp
              trpl '->' Exp
            ']' { TrailInspect $5 $8 $11 $14 $17 $20 $23 $26 $29 }

{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) =
  alexError' p ("parse error at token '" ++ unLex t ++ "'")

parseProgram :: FilePath -> String -> Either String Program
parseProgram = runAlex' parse
}
