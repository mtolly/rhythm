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

-- Whitespace, except newlines.
($white # \n)+ ;

-- Numbers. Longest match rule means N.N is float, not int.
$digit+ { \pn str -> (pn, TValue $ VInt $ NN.fromNumberUnsafe $ read str) }
\-? $digit+ \. $digit+ ('e' $digit+)? { \pn str -> (pn, TValue $ VReal $ decRational str) }
\-? $digit+ e $digit+ { \pn str -> (pn, TValue $ VReal $ decRational str) }

-- Raw keywords.
$alpha ($alpha | $digit)+ { \pn str -> (pn, TValue $ VIdent str) }

-- Quoted strings.
\" ([^\"] | \\\")* \" { \pn str -> (pn, TValue $ VQuoted $ read str) }

-- Other chars.
\{ { \pn str -> (pn, LBrace) }
\} { \pn str -> (pn, RBrace) }
\[ { \pn str -> (pn, LBracket) }
\] { \pn str -> (pn, RBracket) }
\= { \pn str -> (pn, Equals) }
[\n\;] { \pn str -> (pn, Newline) }

{

data Token
  = TValue Value
  | LBrace
  | RBrace
  | LBracket
  | RBracket
  | Equals
  | Newline
  deriving (Eq, Ord, Show)

scan :: String -> [(AlexPosn, Token)]
scan = alexScanTokens

decRational :: String -> Rational
decRational ('-':xs) = negate $ decRational xs
decRational str = realToFrac (read str :: Double) -- TODO: make more precise?

}
