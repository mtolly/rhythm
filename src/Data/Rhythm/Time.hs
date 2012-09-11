{- |

Defines the three basic types for representing musical information:
beats, ticks, and seconds.

  * Beats are rational distances in terms of musical meter, where a measure in
    the music is broken up into a certain number of beats.

  * Ticks are an integer value meant to approximate Beats. Many file formats,
    for practicality, use an integer with a fixed denominator for this purpose.

  * Seconds store each event as fixed at a certain timestamp, a rational value
    in seconds.

-}
module Data.Rhythm.Time where

import qualified Numeric.NonNegative.Wrapper as NN
import qualified Numeric.NonNegative.Class as NN
import qualified Data.EventList.Relative.TimeBody as RTB
import Data.Ratio

-- | A beat, or quarter note, is the most central timekeeping unit.
type Beats = NN.Rational
-- | In file formats such as MIDI, beats are encoded as numerators with a fixed
-- denominator (the 'Resolution').
type Ticks = NN.Integer
-- | Defines how many ticks are in one beat.
type Resolution = Ticks
-- | A precise time position/duration in seconds.
type Seconds = NN.Rational
-- | A tempo, in beats per minute.
type BPM = NN.Rational

fromTicks :: Resolution -> Ticks -> Beats
fromTicks res tks = NN.fromNumberUnsafe $ fromIntegral tks / fromIntegral res

-- | Converts a rational duration to integer ticks. If converting multiple
-- consecutive durations, use 'toTickTrack' instead to avoid rounding error.
toTicks :: Resolution -> Beats -> Ticks
toTicks res bts = NN.fromNumberUnsafe $ floor $ bts * fromIntegral res

fromTickTrack :: Resolution -> RTB.T Ticks a -> RTB.T Beats a
fromTickTrack res = RTB.mapTime (fromTicks res)

-- | Rounds each beat duration to an integral value, correcting for rounding
-- errors.
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
