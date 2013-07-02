{- |
Type definitions for the FeedBack .chart file format. This ad hoc format was
created by TurkeyMan for use with FeedBack, a Guitar Hero customs charting
program.
-}
module Data.Rhythm.FeedBack where

import Data.Rhythm.Time
import Data.Rhythm.Event
import qualified Data.Rhythm.Status as Status
import qualified Numeric.NonNegative.Wrapper as NN
import qualified Numeric.NonNegative.Class as NN
import qualified Data.EventList.Relative.TimeBody as RTB

data Value
  = Int NN.Integer -- ^ Non-negative integer literals, like 0, 8, or 123
  | Real Rational -- ^ Decimal literals, like 0.0, 5.42, or -6789.1234
  | Quoted String -- ^ Quoted strings, like \"hello,\\nworld!\" or \"My Song\"
  | Ident String -- ^ Raw identifiers, like TS, rhythm, or Song
  deriving (Eq, Ord, Show)

data File t a = File
  { songData :: [(String, Value)]
  , syncTrack :: RTB.T t (T a)
  , chunks :: [(String, RTB.T t (T a))] }
  deriving (Eq, Ord, Show)

data Length
  = Note NN.Integer
  | Stream NN.Integer
  deriving (Eq, Ord, Show)

data Point
  = BPM BPS
  | Anchor Seconds
  | TimeSig NN.Integer
  | EventGlobal String
  | EventLocal String
  deriving (Eq, Ord, Show)

instance Long Length
type T = Event Length Point

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

getValue :: String -> File t a -> Maybe Value
getValue str = lookup str . songData

setValue :: String -> Value -> File t a -> File t a
setValue str val f = f { songData = new } where
  new = (str, val) : [p | p@(x, _) <- songData f, x /= str]

getResolution :: File t a -> Maybe Resolution
getResolution f = fmap Ticks $ getValue "Resolution" f >>= fromInt

setResolution :: Resolution -> File t a -> File t a
setResolution = setValue "Resolution" . Int . unTicks

getTempoTrack :: (NN.C t) => File t a -> Status.T t BPS
getTempoTrack = Status.fromRTB 120 . RTB.mapMaybe g . syncTrack where
  g (Point (BPM b)) = Just b
  g _               = Nothing

mapTracks :: (RTB.T t (T a) -> RTB.T u (T b)) -> File t a -> File u b
mapTracks g f = File
  { songData = songData f
  , syncTrack = g $ syncTrack f
  , chunks = map (fmap g) $ chunks f }

unifyFile :: (NN.C t) => File t Bool -> File t t
unifyFile = mapTracks unifyEvents

splitFile :: (NN.C t) => File t t -> File t Bool
splitFile = mapTracks splitEvents

-- | Uses the file's tempo track to convert.
toTimeFile :: File Beats a -> File Seconds a
toTimeFile f = mapTracks (toTimeTrack $ getTempoTrack f) f

-- | Uses the file's tempo track to convert.
fromTimeFile :: File Seconds a -> File Beats a
fromTimeFile f = mapTracks (fromTimeTrack $ getTempoTrack f) f

-- | Uses the file's resolution to convert, or returns Nothing if no resolution.
fromTickFile :: File Ticks a -> Maybe (File Beats a)
fromTickFile f = getResolution f >>= \r -> Just $ mapTracks (fromTickTrack r) f

-- | Uses the file's resolution to convert, or returns Nothing if no resolution.
toTickFile :: File Beats a -> Maybe (File Ticks a)
toTickFile f = getResolution f >>= \r -> Just $ mapTracks (toTickTrack r) f
