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

type Fret = NN.Integer
type SwitchType = NN.Integer

data Value
  = Int NN.Integer -- ^ Non-negative integer literals, like 0, 8, or 123
  | Real Rational -- ^ Floating point literals, like 0.0, 5.42, or -6789.1234
  | Quoted String -- ^ Quoted strings, like \"hello,\\nworld!\" or \"My Song\"
  | Ident String -- ^ Raw identifiers, like TS, rhythm, or Song
  deriving (Eq, Ord, Show)

showValue :: Value -> String
showValue (Int i) = show i
showValue (Real r) = show (fromRational r :: Double)
showValue (Quoted s) = show s
showValue (Ident s) = s

type RawChunk = [(Value, [Value])]
type SongChunk = [(String, Value)]
type TimeChunk t = RTB.T t (Event t)

data File t = File
  { song :: SongChunk
  , sync :: TimeChunk t
  , events :: [(String, TimeChunk t)] }
  deriving (Eq, Ord, Show)

type Event = TimeEvent Duration Point

data Duration
  = Note Fret
  | Switch SwitchType
  deriving (Eq, Ord, Show)

data Point
  = BPM BPM
  | Anchor Seconds
  | TSig NN.Integer
  | EventGlobal String
  | EventLocal String
  deriving (Eq, Ord, Show)

readEvent :: [Value] -> Maybe (Event Ticks)
readEvent (Ident i : rest) = case (i, rest) of
  ("B", [Int b]) -> Just $ Point $ BPM $ fromIntegral b / 1000
  ("A", [Int a]) -> Just $ Point $ Anchor $ fromIntegral a / 1000000
  ("TS", [Int ts]) -> Just $ Point $ TSig ts
  ("E", [Quoted e]) -> Just $ Point $ EventGlobal e
  ("E", [Ident e]) -> Just $ Point $ EventLocal e
  ("N", [Int f, Int d]) -> Just $ Duration (Note f) $ fromIntegral d
  ("S", [Int t, Int d]) -> Just $ Duration (Switch t) $ fromIntegral d
  _ -> Nothing
readEvent _ = Nothing

showEvent :: Event Ticks -> [Value]
showEvent e = case e of
  Point p -> case p of
    BPM b -> [Ident "B", Int $ floor $ b * 1000]
    Anchor a -> [Ident "A", Int $ floor $ a * 1000000]
    TSig ts -> [Ident "TS", Int ts]
    EventGlobal str -> [Ident "E", Quoted str]
    EventLocal str -> [Ident "E", Ident str]
  Duration d tks -> case d of
    Note f -> [Ident "N", Int f, Int $ fromIntegral tks]
    Switch t -> [Ident "S", Int t, Int $ fromIntegral tks]

showChunk :: String -> RawChunk -> String
showChunk name evs = concat
  [ "[", name, "]\n"
  , "{\n"
  , concatMap showLine evs
  , "}\n" ] where
  showLine (lv, rvs) = concat
    ["  ", showValue lv, " = ", unwords (map showValue rvs), "\n"]
