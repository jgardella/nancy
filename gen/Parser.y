{
module Parser where

import Data.Char
import qualified Lexer as L
import qualified Types as T
}

%name calc
%tokentype { L.Token }
%error { parseError }

%token
  id    { L.Var $$ }
  num   { L.Num $$ }
  fun   { L.Fun }
  '->'  { L.Arrow }
  '('   { L.LParen }
  ')'   { L.RParen }
  '{'   { L.LBrace }
  '}'   { L.RBrace }
  '<'   { L.LBrack }
  '>'   { L.RBrack }
  ';'   { L.Semi }
  ':'   { L.Colon }
  '!'   { L.Bang }
  '.'   { L.Dot }
  let   { L.Let }
  be    { L.Be }
  in    { L.In }
  trl   { L.Trail }
  int   { L.TInt }
  bool  { L.TBool }
  true  { L.TTrue }
  false { L.TFalse }


%%

Program   : Exp                              { Program $1 }
Type      : int                              { T.Int }
          | bool                             { T.Bool }
          | Type '->' Type                   { T.Arrow $1 $3 }
Exp       : id                               { Id $1 }
          | num                              { Number $1 }
          | true                             { Boolean True }
          | false                            { Boolean False }
          | fun id ':' Type '.' '{' Exp '}'  { Abs $2 $4 $7 }
          | Exp Exp                          { App $1 $2 }
          | '<' id ';' id '>'                { AuditedVar $2 $4 }
          | '!' id Exp                       { AuditedUnit $2 $3 }
          | let id be Exp in Exp             { AuditedComp $2 $4 $6 }
          | trl '<' Exp '>'                  { TrailInspect $3 }

{

parseError :: [L.Token] -> a
parseError _ = error "Parse error"

data Program = Program Exp
  deriving Show

data Exp
  = Id String
  | Number Int
  | Boolean Bool
  | Abs String T.Type Exp
  | App Exp Exp
  | AuditedVar String String
  | AuditedUnit String Exp
  | AuditedComp String Exp Exp
  | TrailInspect Exp
  deriving Show
}
