{
{-# OPTIONS_GHC -w #-}
-- | Generated lexer for FeedBack .chart format
module Data.Rhythm.FeedBack.Lex (scan, Token(..), AlexPosn(..)) where

import qualified Numeric.NonNegative.Wrapper as NN
import Data.Rhythm.FeedBack
import Numeric
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

-- Whitespace.
$white+ ;

-- Numbers. Int rule comes first to ensure ints will be Int, not Real.
$digit+
  { \pn str -> (pn, TValue $ Int $ NN.fromNumberUnsafe $ read str) }
\-? $digit+ (\. $digit+)? (e $digit+)?
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

-- Punctuation.
\{ { \pn _ -> (pn, LBrace) }
\} { \pn _ -> (pn, RBrace) }
\[ { \pn _ -> (pn, LBracket) }
\] { \pn _ -> (pn, RBracket) }
\= { \pn _ -> (pn, Equals) }

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
decRational str = case readSigned readFloat str of
  ((r, _):_) -> r
  _ -> error $ "decRational: not a valid number format: " ++ str

}