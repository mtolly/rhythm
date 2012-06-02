{- |
Datatype definitions for the FeedBack .chart file format. This ad hoc format was
created by TurkeyMan for use with FeedBack, his custom Guitar Hero charting
program. Instead of note on/off events like in MIDI, each note is stored with a
duration value. The .chart file also stores various metadata about the song.
-}
module Data.FeedBack.Base where

import Data.MusicTime
import qualified Numeric.NonNegative.Wrapper as NN
import qualified Data.EventList.Relative.TimeBody as RTB

data Value
  = Int NN.Integer -- ^ Non-negative integer literals, like 0, 8, or 123
  | Real Rational -- ^ Floating point literals, like 0.0, 5.42, or -6789.1234
  | Quoted String -- ^ Quoted strings, like \"hello,\\nworld!\" or \"My Song\"
  | Ident String -- ^ Raw identifiers, like TS, rhythm, or Song
  deriving (Eq, Ord, Show)

type SongChunk = [(String, Value)]
type EventChunk t = RTB.T t (Event t)

data File t = File
  { songChunk :: SongChunk
  , syncTrack :: EventChunk t
  , eventChunks :: [(String, EventChunk t)] }
  deriving (Eq, Ord, Show)

type Event = TimeEvent Duration Point

data Duration
  = Note Fret
  | Stream StreamType
  deriving (Eq, Ord, Show)

type Fret = NN.Integer
type StreamType = NN.Integer

data Point
  = BPM BPM
  | Anchor Seconds
  | TSig NN.Integer
  | EventGlobal String
  | EventLocal String
  deriving (Eq, Ord, Show)
