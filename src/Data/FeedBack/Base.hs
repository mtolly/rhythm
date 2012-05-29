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

-- | FeedBack .chart files use four basic data types.
data Value
  -- | Non-negative integer literals, like 0, 8, or 123
  = VInt NN.Integer
  -- | Floating point literals, like 0.0, 5.42, or -6789.1234
  | VReal Rational
  -- | Quoted strings, like \"hello,\\nworld!\" or \"My Song\"
  | VQuoted String
  -- | Raw identifiers, like TS, rhythm, or Song
  | VIdent String
  deriving (Eq, Ord, Show)

showValue :: Value -> String
showValue (VInt i) = show i
showValue (VReal r) = show (fromRational r :: Double)
showValue (VQuoted s) = show s
showValue (VIdent s) = s

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
readEvent (VIdent i : rest) = case (i, rest) of
  ("B", [VInt b]) -> Just $ Point $ BPM $ fromIntegral b / 1000
  ("A", [VInt a]) -> Just $ Point $ Anchor $ fromIntegral a / 1000000
  ("TS", [VInt ts]) -> Just $ Point $ TSig ts
  ("E", [VQuoted e]) -> Just $ Point $ EventGlobal e
  ("E", [VIdent e]) -> Just $ Point $ EventLocal e
  ("N", [VInt f, VInt d]) -> Just $ Duration (Note f) $ fromIntegral d
  ("S", [VInt t, VInt d]) -> Just $ Duration (Switch t) $ fromIntegral d
  _ -> Nothing
readEvent _ = Nothing

showEvent :: Event Ticks -> [Value]
showEvent e = case e of
  Point p -> case p of
    BPM b -> [VIdent "B", VInt $ floor $ b * 1000]
    Anchor a -> [VIdent "A", VInt $ floor $ a * 1000000]
    TSig ts -> [VIdent "TS", VInt ts]
    EventGlobal str -> [VIdent "E", VQuoted str]
    EventLocal str -> [VIdent "E", VIdent str]
  Duration d tks -> case d of
    Note f -> [VIdent "N", VInt f, VInt $ fromIntegral tks]
    Switch t -> [VIdent "S", VInt t, VInt $ fromIntegral tks]

showChunk :: String -> RawChunk -> String
showChunk name evs = "[" ++ name ++ "]\n"
  ++ "{\n"
  ++ concatMap showLine evs
  ++ "}\n" where
  showLine (lv, rvs) =
    "  " ++ showValue lv ++ " = " ++ unwords (map showValue rvs) ++ "\n"
