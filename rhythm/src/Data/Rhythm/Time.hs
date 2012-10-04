{- |
Defines basic types for musical information.
-}
module Data.Rhythm.Time where

import qualified Numeric.NonNegative.Wrapper as NN
import qualified Numeric.NonNegative.Class as NN
import qualified Data.EventList.Relative.TimeBody as RTB
import Data.Ratio
import qualified Data.Rhythm.Status as Status

-- | A precise rational number of beats (quarter notes).
type Beats = NN.Rational
-- | An approximation of 'Beats': an integer numerator with a fixed denominator.
type Ticks = NN.Integer
-- | The denominator of a 'Ticks' value: defines how many ticks are in one beat.
type Resolution = Ticks
-- | A time position/duration in seconds.
type Seconds = NN.Rational
-- | A tempo, in beats per minute.
type BPM = NN.Rational

fromTicks :: Resolution -> Ticks -> Beats
fromTicks res tks = NN.fromNumberUnsafe $ fromIntegral tks / fromIntegral res

-- | If converting multiple consecutive durations, use 'toTickTrack' instead to
-- avoid rounding error.
toTicks :: Resolution -> Beats -> Ticks
toTicks res bts = NN.fromNumberUnsafe $ floor $ bts * fromIntegral res

fromTickTrack :: Resolution -> RTB.T Ticks a -> RTB.T Beats a
fromTickTrack res = RTB.mapTime (fromTicks res)

-- | Corrects for rounding errors along the way.
toTickTrack :: Resolution -> RTB.T Beats a -> RTB.T Ticks a
toTickTrack res = RTB.discretize .
  RTB.mapTime (* (NN.fromNumberUnsafe $ fromIntegral res))

-- | The smallest resolution needed to represent all times correctly.
minResolution :: [Beats] -> Resolution
minResolution = NN.fromNumberUnsafe . foldr (lcm . denominator . NN.toNumber) 1

-- | The smallest resolution needed to represent all event positions correctly.
minTrackResolution :: RTB.T Beats a -> Resolution
minTrackResolution = minResolution . RTB.getTimes

toTime :: BPM -> Beats -> Seconds
toTime bpm bts = (bts / bpm) * 60

fromTime :: BPM -> Seconds -> Beats
fromTime bpm secs = (secs / 60) * bpm

-- | Each event-list is merged into a new list, starting at its position in the
-- original list. This is equivalent to the monad function join, but the Monad
-- typeclass can't be used because of typeclass constraints.
rtbJoin :: (NN.C t, Ord a) => RTB.T t (RTB.T t a) -> RTB.T t a
rtbJoin rtb = case RTB.viewL rtb of
  Nothing -> RTB.empty
  Just ((dt, x), rtb') -> RTB.delay dt $ RTB.merge x $ rtbJoin rtb'

-- | Uses tempos to convert an event-list from beatstamps to timestamps.
toTimeTrack :: Status.T Beats BPM -> RTB.T Beats a -> RTB.T Seconds a
toTimeTrack = Status.applyTime . fmap toTime

-- | Uses tempos to convert an event-list from timestamps to beatstamps.
fromTimeTrack :: Status.T Seconds BPM -> RTB.T Seconds a -> RTB.T Beats a
fromTimeTrack = Status.applyTime . fmap fromTime

toTickStatus :: Resolution -> Status.T Beats a -> Status.T Ticks a
toTickStatus res =
  uncurry Status.fromRTB . fmap (toTickTrack res) . Status.toRTB

fromTickStatus :: Resolution -> Status.T Ticks a -> Status.T Beats a
fromTickStatus res =
  uncurry Status.fromRTB . fmap (fromTickTrack res) . Status.toRTB
