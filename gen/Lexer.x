{
{-# OPTIONS -w  #-}
module Nancy.Lexer
  ( Token(..)
  , AlexPosn(..)
  , TokenClass(..)
  , unLex
  , Alex(..)
  , runAlex'
  , alexMonadScan'
  , alexError'
  ) where

import Prelude hiding (lex)
import Control.Monad ( liftM )
}
%wrapper "monadUserState"

$digit = 0-9			  -- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-
  $white+                         ;
  "//".*                          ;
  fun                             { lex' TokenFun }
  inspect                         { lex' TokenInspect }
  "->"                            { lex' TokenArrow }
  "<"                             { lex' TokenLBrack }
  ">"                             { lex' TokenRBrack }
  "{"                             { lex' TokenLBrace }
  "}"                             { lex' TokenRBrace }
  "("                             { lex' TokenLParen }
  ")"                             { lex' TokenRParen }
  "["                             { lex' TokenLSquare }
  "]"                             { lex' TokenRSquare }
  ":"                             { lex' TokenColon }
  ";"                             { lex' TokenSemi }
  "!"                             { lex' TokenBang }
  "."                             { lex' TokenDot }
  "r"                             { lex' TokenR }
  "t"                             { lex' TokenT }
  ba                              { lex' TokenBA }
  bb                              { lex' TokenBB }
  ti                              { lex' TokenTI }
  lam                             { lex' TokenLAM }
  app                             { lex' TokenAPP }
  trpl                            { lex' TokenTRPL }
  let                             { lex' TokenLet }
  be                              { lex' TokenBe }
  in                              { lex' TokenIn }
  int                             { lex' TokenInt }
  bool                            { lex' TokenBool }
  true                            { lex' TokenTrue }
  false                           { lex' TokenFalse }
  $alpha [$alpha $digit \_ \']*		{ lex  TokenVar }
  $digit+                         { lex (TokenNum . read) }

{
-- To improve error messages, We keep the path of the file we are
-- lexing in our own state.
data AlexUserState = AlexUserState { filePath :: FilePath }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>"

getFilePath :: Alex FilePath
getFilePath = liftM filePath alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath = alexSetUserState . AlexUserState

-- The token type, consisting of the source code position and a token class.
data Token = Token AlexPosn TokenClass
  deriving ( Show )

data TokenClass
  = TokenFun
  | TokenInspect
  | TokenArrow
  | TokenLBrack
  | TokenRBrack
  | TokenLBrace
  | TokenRBrace
  | TokenLParen
  | TokenRParen
  | TokenLSquare
  | TokenRSquare
  | TokenDot
  | TokenBang
  | TokenColon
  | TokenSemi
  | TokenR
  | TokenT
  | TokenBA
  | TokenBB
  | TokenTI
  | TokenLAM
  | TokenAPP
  | TokenTRPL
  | TokenLet
  | TokenBe
  | TokenIn
  | TokenTrue
  | TokenFalse
  | TokenInt
  | TokenBool
  | TokenVar String
  | TokenNum Int
  | TokenEOF
  deriving ( Show )

-- For nice parser error messages.
unLex :: TokenClass -> String
unLex t = show t

alexEOF :: Alex Token
alexEOF = do
  (p,_,_,_) <- alexGetInput
  return $ Token p TokenEOF

-- Unfortunately, we have to extract the matching bit of string
-- ourselves...
lex :: (String -> TokenClass) -> AlexAction Token
lex f = \(p,_,_,s) i -> return $ Token p (f (take i s))

-- For constructing tokens that do not depend on
-- the input
lex' :: TokenClass -> AlexAction Token
lex' = lex . const

-- We rewrite alexMonadScan' to delegate to alexError' when lexing fails
-- (the default implementation just returns an error message).
alexMonadScan' :: Alex Token
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (p, _, _, s) ->
        alexError' p ("lexical error at character '" ++ take 1 s ++ "'")
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

-- Signal an error, including a commonly accepted source code position.
alexError' :: AlexPosn -> String -> Alex a
alexError' (AlexPn _ l c) msg = do
  fp <- getFilePath
  alexError (fp ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ msg)

-- A variant of runAlex, keeping track of the path of the file we are lexing.
runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a fp input = runAlex input (setFilePath fp >> a)
}
