{
{-# OPTIONS_GHC -w #-}
-- | Generated lexer for FeedBack .chart format
module Data.FeedBack.Lex (scan, Token(..), AlexPosn(..)) where

import qualified Numeric.NonNegative.Wrapper as NN
import Data.FeedBack.Base
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

-- Whitespace.
$white+ ;

-- Numbers. Longest match rule means N.N is float, not int.
$digit+
  { \pn str -> (pn, TValue $ Int $ NN.fromNumberUnsafe $ read str) }
\-? $digit+ \. $digit+ ('e' $digit+)?
  { \pn str -> (pn, TValue $ Real $ decRational str) }
\-? $digit+ e $digit+
  { \pn str -> (pn, TValue $ Real $ decRational str) }

-- Reserved words.
Song { \pn _ -> (pn, Song) }
SyncTrack { \pn _ -> (pn, SyncTrack) }
B { \pn _ -> (pn, B) }
A { \pn _ -> (pn, A) }
TS { \pn _ -> (pn, TS) }
E { \pn _ -> (pn, E) }
N { \pn _ -> (pn, N) }
S { \pn _ -> (pn, S) }

-- Raw keywords.
$alpha ($alpha | $digit)+ { \pn str -> (pn, TValue $ Ident str) }

-- Quoted strings.
\" ([^\"] | \\\")* \" { \pn str -> (pn, TValue $ Quoted $ read str) }

-- Other chars.
\{ { \pn str -> (pn, LBrace) }
\} { \pn str -> (pn, RBrace) }
\[ { \pn str -> (pn, LBracket) }
\] { \pn str -> (pn, RBracket) }
\= { \pn str -> (pn, Equals) }

{

data Token
  = TValue Value
  | LBrace
  | RBrace
  | LBracket
  | RBracket
  | Equals
  | Song
  | SyncTrack
  | B
  | A
  | TS
  | E
  | N
  | S
  deriving (Eq, Ord, Show)

scan :: String -> [(AlexPosn, Token)]
scan = alexScanTokens

decRational :: String -> Rational
decRational ('-':xs) = negate $ decRational xs
decRational str = realToFrac (read str :: Double) -- TODO: make more precise?

}