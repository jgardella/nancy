{
{-# OPTIONS -w #-}
module AudiComp.Parser( parseProgram ) where

import Data.Char
import AudiComp.Lexer
import AudiComp.Core.Language
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
Type      : int                              { IntT }
          | bool                             { BoolT }
          | Type '->' Type                   { ArrowT $1 $3 }
R         : 'r' '->' Exp                     { ReflexivityM $3 }
S         : 's' '(' id ')' '->' Exp          { SymmetryM $3 $6 }
T         : 't' '(' id id ')' '->' Exp       { TransitivityM $3 $4 $7}
BA        : ba '->' Exp                      { BetaM $3 }
BB        : bb '->' Exp                      { BetaBoxM $3 }
TI        : ti '->' Exp                      { TrailInspectionM $3 }
ABS       : abs '(' id ')' '->' Exp          { AbstractionM $3 $6 }
APP       : app '(' id id ')' '->' Exp       { ApplicationM $3 $4 $7 }
LET       : let '(' id id ')' '->' Exp       { LetM $3 $4 $7 }
TRPL      : trpl '(' id id id id id id id id id id ')' '->' Exp { ReplacementM $3 $4 $5 $6 $7 $8 $9 $10 $11 $12 $15 }
Rename    : id '->' id                       { TrailRename $1 $3 }
Renames   : Rename                           { [$1] }
          | Renames ';' Rename               { $3 : $1 }
Exp       : id                               { Id $1 }
          | num                              { Number $1 }
          | true                             { Boolean True }
          | false                            { Boolean False }
          | '(' Exp ')'                      { Brack $2 }
          | fun '(' id ':' Type ')' '->' Exp { Abs $3 $5 $8 }
          | Exp Exp                          { App $1 $2 }
          | '<' Renames '|' id '>'           { AuditedVar $2 $4 }
          | '!' id Exp                       { AuditedUnit $2 $3 }
          | let id ':' Type be Exp in Exp    { AuditedComp $2 $4 $6 $8 }
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
