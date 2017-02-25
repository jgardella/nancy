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
  '['   { L.LSquare }
  ']'   { L.RSquare }
  '{'   { L.LBrace }
  '}'   { L.RBrace }
  '<'   { L.LBrack }
  '>'   { L.RBrack }
  ';'   { L.Semi }
  ':'   { L.Colon }
  '!'   { L.Bang }
  '.'   { L.Dot }
  '|'   { L.Bar }
  'r'   { L.R }
  's'   { L.S }
  't'   { L.T }
  ba    { L.BA }
  bb    { L.BB }
  ti    { L.TI }
  abs   { L.ABS }
  app   { L.APP }
  trpl  { L.TRPL }
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
          | '!' Type                         { T.Audited $2 }
R         : 'r' '(' id ')' '.' Exp           { Reflexivity $3 $6 }
S         : 's' '(' id ')' '.' Exp           { Symmetry $3 $6 }
T         : 't' '(' id id ')' '.' Exp        { Transitivity $3 $4 $7}
BA        : ba '(' id id ')' '.' Exp         { Beta $3 $4 $7 }
BB        : bb '(' id id ')' '.' Exp         { BetaBox $3 $4 $7 }
TI        : ti '(' id id ')' '.' Exp         { TrailInspection $3 $4 $7 }
ABS       : abs '(' id ')' '.' Exp           { Abstraction $3 $6 }
APP       : app '(' id id ')' '.' Exp        { Application $3 $4 $7 }
LET       : let '(' id id ')' '.' Exp        { Let $3 $4 $7 }
TRPL      : trpl '(' id id id id id id id id id id ')' '.' Exp { Replacement $3 $4 $5 $6 $7 $8 $9 $10 $11 $12 $15 }
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

parseError :: [L.Token] -> a
parseError _ = error "Parse error"

data Program = Program Exp
  deriving Show

data Exp
  = Id String
  | Number Int
  | Boolean Bool
  | Brack Exp
  | Abs String T.Type Exp
  | App Exp Exp
  | AuditedVar String String String
  | AuditedUnit String Exp
  | AuditedComp String Exp Exp
  | TrailInspect String TrailMap TrailMap TrailMap TrailMap TrailMap TrailMap TrailMap TrailMap TrailMap TrailMap
  | DerivedTerm T.Trail Exp
  deriving Show

data TrailMap
  = Reflexivity String Exp
  | Symmetry String Exp
  | Transitivity String String Exp
  | Beta String String Exp
  | BetaBox String String Exp
  | TrailInspection String String Exp
  | Abstraction String Exp
  | Application String String Exp
  | Let String String Exp
  | Replacement String String String String String String String String String String Exp
  deriving Show
}
