{- |
Datatype definitions for the FeedBack .chart file format. This ad hoc format was
created by TurkeyMan for use with FeedBack, his custom Guitar Hero charting
program. Instead of note on/off events like in MIDI, each note is stored with a
duration value. The .chart file also stores various metadata about the song.
-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Rhythm.FeedBack where

import Data.Rhythm.Time
import Data.Rhythm.Event
import qualified Numeric.NonNegative.Wrapper as NN
import qualified Data.EventList.Relative.TimeBody as RTB
import Control.Monad ((>=>))

data Value
  = Int NN.Integer -- ^ Non-negative integer literals, like 0, 8, or 123
  | Real Rational -- ^ Floating point literals, like 0.0, 5.42, or -6789.1234
  | Quoted String -- ^ Quoted strings, like \"hello,\\nworld!\" or \"My Song\"
  | Ident String -- ^ Raw identifiers, like TS, rhythm, or Song
  deriving (Eq, Ord, Show)

type SongData = [(String, Value)]
type Chunk t = RTB.T t (T t)

data File t = File
  { songData :: SongData
  , syncTrack :: Chunk t
  , chunks :: [(String, Chunk t)] }
  deriving (Eq, Ord, Show)

data Long
  = Note NN.Integer
  | Stream NN.Integer
  deriving (Eq, Ord, Show)

data Point
  = BPM BPM
  | Anchor Seconds
  | TimeSig NN.Integer
  | EventGlobal String
  | EventLocal String
  deriving (Eq, Ord, Show)

instance Duration Long Point
type T = Event Long Point

fromInt :: Value -> Maybe NN.Integer
fromInt (Int i) = Just i
fromInt _ = Nothing

fromReal :: Value -> Maybe Rational
fromReal (Real r) = Just r
fromReal _ = Nothing

fromQuoted :: Value -> Maybe String
fromQuoted (Quoted s) = Just s
fromQuoted _ = Nothing

fromIdent :: Value -> Maybe String
fromIdent (Ident s) = Just s
fromIdent _ = Nothing

getValue :: String -> File a -> Maybe Value
getValue str = lookup str . songData

setValue :: String -> Value -> File a -> File a
setValue str val f = f { songData = new } where
  new = (str, val) : [p | p@(x, _) <- songData f, x /= str]

getResolution :: File a -> Maybe Resolution
getResolution = getValue "Resolution" >=> fromInt

setResolution :: Resolution -> File a -> File a
setResolution = setValue "Resolution" . Int

fileToBeats :: File Ticks -> Maybe (File Beats)
fileToBeats f = getResolution f >>= \res -> Just $ File
  { songData = songData f
  , syncTrack = toBeatTrack' res $ syncTrack f
  , chunks = [(name, toBeatTrack' res t) | (name, t) <- chunks f] }

fileToTicks :: File Beats -> Maybe (File Ticks)
fileToTicks f = getResolution f >>= \res -> Just $ File
  { songData = songData f
  , syncTrack = toTickTrack' res $ syncTrack f
  , chunks = [(name, toTickTrack' res t) | (name, t) <- chunks f] }
