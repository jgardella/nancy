{
{-# OPTIONS -w #-}
module Parser( parseProgram ) where

import Data.Char
import Lexer
import Language
import qualified Types as T
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
  '->'  { Token _ TokenArrow }
  '('   { Token _ TokenLParen }
  ')'   { Token _ TokenRParen }
  '['   { Token _ TokenLSquare }
  ']'   { Token _ TokenRSquare }
  '{'   { Token _ TokenLBrace }
  '}'   { Token _ TokenRBrace }
  '<'   { Token _ TokenLBrack }
  '>'   { Token _ TokenRBrack }
  ';'   { Token _ TokenSemi }
  ':'   { Token _ TokenColon }
  '!'   { Token _ TokenBang }
  '.'   { Token _ TokenDot }
  '|'   { Token _ TokenBar }
  'r'   { Token _ TokenR }
  's'   { Token _ TokenS }
  't'   { Token _ TokenT }
  ba    { Token _ TokenBA }
  bb    { Token _ TokenBB }
  ti    { Token _ TokenTI }
  abs   { Token _ TokenABS }
  app   { Token _ TokenAPP }
  trpl  { Token _ TokenTRPL }
  let   { Token _ TokenLet }
  be    { Token _ TokenBe }
  in    { Token _ TokenIn }
  trl   { Token _ TokenTrail }
  int   { Token _ TokenInt }
  bool  { Token _ TokenBool }
  true  { Token _ TokenTrue }
  false { Token _ TokenFalse }


%%

Program   : Exp                              { Program $1 }
Type      : int                              { T.Int }
          | bool                             { T.Bool }
          | Type '->' Type                   { T.Arrow $1 $3 }
          | '!' Type                         { T.Audited $2 }
R         : 'r' '(' id ')' '->' Exp          { Reflexivity $3 $6 }
S         : 's' '(' id ')' '->' Exp          { Symmetry $3 $6 }
T         : 't' '(' id id ')' '->' Exp       { Transitivity $3 $4 $7}
BA        : ba '(' id id ')' '->' Exp        { Beta $3 $4 $7 }
BB        : bb '(' id id ')' '->' Exp        { BetaBox $3 $4 $7 }
TI        : ti '(' id id ')' '->' Exp        { TrailInspection $3 $4 $7 }
ABS       : abs '(' id ')' '->' Exp          { Abstraction $3 $6 }
APP       : app '(' id id ')' '->' Exp       { Application $3 $4 $7 }
LET       : let '(' id id ')' '->' Exp       { Let $3 $4 $7 }
TRPL      : trpl '(' id id id id id id id id id id ')' '->' Exp { Replacement $3 $4 $5 $6 $7 $8 $9 $10 $11 $12 $15 }
Exp       : id                               { Id $1 }
          | num                              { Number $1 }
          | true                             { Boolean True }
          | false                            { Boolean False }
          | '(' Exp ')'                      { Brack $2 }
          | fun '(' id ':' Type ')' '->' Exp { Abs $3 $5 $8 }
          | Exp Exp                          { App $1 $2 }
          | '<' id ';' id '|' id '>'         { AuditedVar $2 $4 $6 }
          | '!' id Exp                       { AuditedUnit $2 $3 }
          | let id be Exp in Exp             { AuditedComp $2 $4 $6 }
          | id '[' R ';' S ';' T ';' BA ';' BB ';' TI ';' ABS ';' APP ';' LET ';' TRPL ']' { TrailInspect $1 $3 $5 $7 $9 $11 $13 $15 $17 $19 $21 }

{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) =
  alexError' p ("parse error at token '" ++ unLex t ++ "'")

parseProgram :: FilePath -> String -> Either String Program
parseProgram = runAlex' parse
}
