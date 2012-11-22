{- |
Defines basic types for musical information.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Rhythm.Time where

import qualified Numeric.NonNegative.Wrapper as NN
import qualified Numeric.NonNegative.Class as NN
import qualified Data.EventList.Relative.TimeBody as RTB
import Data.Ratio
import qualified Data.Rhythm.Status as Status
import Data.Monoid (Monoid)

-- | A precise rational number of beats (quarter notes).
newtype Beats = Beats { unBeats :: NN.Rational }
  deriving (Eq, Ord, Show, Num, Real, Fractional, RealFrac, NN.C, Monoid)
-- | An approximation of 'Beats': an integer numerator with a fixed denominator.
newtype Ticks = Ticks { unTicks :: NN.Integer }
  deriving (Eq, Ord, Show, Enum, Num, Real, Integral, NN.C, Monoid)
-- | The denominator of a 'Ticks' value: defines how many ticks are in one beat.
type Resolution = Ticks
-- | A time position/duration in seconds.
newtype Seconds = Seconds { unSeconds :: NN.Rational }
  deriving (Eq, Ord, Show, Num, Real, Fractional, RealFrac, NN.C, Monoid)
-- | A tempo, in beats per minute.
type BPM = Beats

fromTicks :: Resolution -> Ticks -> Beats
fromTicks (Ticks res) (Ticks tks) =
  Beats $ NN.fromNumberUnsafe $ fromIntegral tks / fromIntegral res

-- | If converting multiple consecutive durations, use 'toTickTrack' instead to
-- avoid rounding error.
toTicks :: Resolution -> Beats -> Ticks
toTicks (Ticks res) (Beats bts) =
  Ticks $ NN.fromNumberUnsafe $ floor $ bts * fromIntegral res

fromTickTrack :: Resolution -> RTB.T Ticks a -> RTB.T Beats a
fromTickTrack res = RTB.mapTime (fromTicks res)

-- | Corrects for rounding errors along the way.
toTickTrack :: Resolution -> RTB.T Beats a -> RTB.T Ticks a
toTickTrack (Ticks res) = RTB.discretize . RTB.mapTime f where
  f (Beats bts) = bts * NN.fromNumberUnsafe (fromIntegral res)

-- | The smallest resolution needed to represent all times correctly.
minResolution :: [Beats] -> Resolution
minResolution = Ticks . NN.fromNumberUnsafe .
  foldr (lcm . denominator . NN.toNumber . unBeats) 1

-- | The smallest resolution needed to represent all event positions correctly.
minTrackResolution :: RTB.T Beats a -> Resolution
minTrackResolution = minResolution . RTB.getTimes

toTime :: BPM -> Beats -> Seconds
toTime (Beats bpm) (Beats bts) = Seconds $ (bts / bpm) * 60

fromTime :: BPM -> Seconds -> Beats
fromTime (Beats bpm) (Seconds secs) = Beats $ (secs / 60) * bpm

-- | Uses tempos to convert an event-list from beatstamps to timestamps.
toTimeTrack :: Status.T Beats BPM -> RTB.T Beats a -> RTB.T Seconds a
toTimeTrack = Status.applyTime . fmap toTime

-- | Uses tempos to convert an event-list from timestamps to beatstamps.
fromTimeTrack :: Status.T Seconds BPM -> RTB.T Seconds a -> RTB.T Beats a
fromTimeTrack = Status.applyTime . fmap fromTime

toTickStatus :: Resolution -> Status.T Beats a -> Status.T Ticks a
toTickStatus = Status.mapRTB . toTickTrack

fromTickStatus :: Resolution -> Status.T Ticks a -> Status.T Beats a
fromTickStatus = Status.mapRTB . fromTickTrack

-- | \"Undoing\" a non-negative number wrapper by adding a sign to it.
data Offset a = Negative a | Positive a
  deriving (Show, Read)

instance (NN.C a) => Eq (Offset a) where
  Negative x == Negative y = x == y
  Positive x == Positive y = x == y
  Negative x == Positive y = NN.zero == x && NN.zero == y
  Positive x == Negative y = NN.zero == x && NN.zero == y

instance (NN.C a) => Ord (Offset a) where
  Positive x <= Positive y = x <= y
  Negative x <= Negative y = y <= x
  Negative _ <= Positive _ = True
  x@(Positive _) <= y@(Negative _) = x == y
