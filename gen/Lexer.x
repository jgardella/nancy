{
module Lexer where
}

%wrapper "basic"

$digit = 0-9			  -- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+                         ;
  "//".*                          ;
  fun                             { \s -> Fun }
  "->"                            { \s -> Arrow }
  "{"                             { \s -> LBrace }
  "}"                             { \s -> RBrace }
  "("                             { \s -> LParen }
  ")"                             { \s -> RParen }
  "["                             { \s -> LSquare }
  "]"                             { \s -> RSquare }
  "<"                             { \s -> LBrack }
  ">"                             { \s -> RBrack }
  ";"                             { \s -> Semi }
  ":"                             { \s -> Colon }
  "!"                             { \s -> Bang }
  "."                             { \s -> Dot }
  "|"                             { \s -> Bar }
  'r'                             { \s -> R }
  's'                             { \s -> S }
  't'                             { \s -> T }
  ba                              { \s -> BA }
  bb                              { \s -> BB }
  ti                              { \s -> TI }
  abs                             { \s -> ABS }
  app                             { \s -> APP }
  trpl                            { \s -> TRPL }
  let                             { \s -> Let }
  be                              { \s -> Be }
  in                              { \s -> In }
  trl                             { \s -> Trail }
  int                             { \s -> TInt }
  bool                            { \s -> TBool }
  true                            { \s -> TTrue }
  false                           { \s -> TFalse }
  $alpha [$alpha $digit \_ \']*		{ \s -> Var s }
  $digit+                         { \s -> Num $ read s }

{
-- Each action has type :: String -> Token

-- The token type:
data Token
  = Fun
  | Arrow
  | LBrace
  | RBrace
  | LParen
  | RParen
  | LSquare
  | RSquare
  | LBrack
  | RBrack
  | Semi
  | Colon
  | Dot
  | Bar
  | Bang
  | R
  | S
  | T
  | BA
  | BB
  | TI
  | ABS
  | APP
  | TRPL
  | Let
  | Be
  | In
  | TTrue
  | TFalse
  | Trail
  | TInt
  | TBool
  | Var String
  | Num Int
  deriving (Eq,Show)

lexer = alexScanTokens
}
