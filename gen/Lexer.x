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
  "<"                             { \s -> LBrack }
  ">"                             { \s -> RBrack }
  ";"                             { \s -> Semi }
  ":"                             { \s -> Colon }
  "!"                             { \s -> Bang }
  "."                             { \s -> Dot }
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
  | LBrack
  | RBrack
  | Semi
  | Colon
  | Dot
  | Bang
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
