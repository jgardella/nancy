{
{-# OPTIONS -w #-}
module Nancy.Parser( parseProgram ) where

import Data.Char
import Nancy.Lexer
import Nancy.Core.Language
import Nancy.Core.Util
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
  '='   { Token _ TokenEq }
  '->'  { Token _ TokenArrow }
  '<'   { Token _ TokenLBrack }
  '>'   { Token _ TokenRBrack }
  '('   { Token _ TokenLParen }
  ')'   { Token _ TokenRParen }
  '{'   { Token _ TokenLBrace }
  '}'   { Token _ TokenRBrace }
  ':'   { Token _ TokenColon }
  '!'   { Token _ TokenBang }
  'r'   { Token _ TokenR }
  't'   { Token _ TokenT }
  ba    { Token _ TokenBA }
  bb    { Token _ TokenBB }
  ti    { Token _ TokenTI }
  lam   { Token _ TokenLAM }
  app   { Token _ TokenAPP }
  trpl  { Token _ TokenTRPL }
  let   { Token _ TokenLet }
  'let!'{ Token _ TokenLetBang }
  in    { Token _ TokenIn }
  int   { Token _ TokenInt }
  bool  { Token _ TokenBool }
  true  { Token _ TokenTrue }
  false { Token _ TokenFalse }

%right in
%right '->'
%nonassoc id num true false '(' fun '<' '!' let 'let!' insp
%nonassoc APP

%%

Program   : Expr                                { Program $1 }
Type      : int                                 { IntType }
          | bool                                { BoolType }
          | Type '->' Type                      { LamType $1 $3 }
Expr      : id                                  { Var $1 }
          | num                                 { Number $1 }
          | true                                { Boolean True }
          | false                               { Boolean False }
          | '(' Expr ')'                        { Brack $2 }
          | Expr Expr %prec APP                 { App $1 $2 }
          | fun id ':' Type '->' Expr           { Lam $2 $4 $6 }
          | '<' id '>'                          { AVar $2 }
          | '!' Expr                            { Bang $2 (RTrail (getWit $2)) }
          | let id ':' Type '=' Expr in Expr    { App (Lam $2 $4 $8) $6 }
          | 'let!' id ':' Type '=' Expr in Expr { ALet $2 $4 $6 $8 }
          | insp '{'
              'r' '->' Expr
              't' '->' Expr
              ba '->' Expr
              bb '->' Expr
              ti '->' Expr
              lam '->' Expr
              app '->' Expr
              let '->' Expr
              trpl '->' Expr
            '}' { Inspect (TrailBranches $5 $8 $11 $14 $17 $20 $23 $26 $29) }

{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) =
  alexError' p ("parse error at token '" ++ unLex t ++ "'")

parseProgram :: FilePath -> String -> Either String Program
parseProgram = runAlex' parse
}
