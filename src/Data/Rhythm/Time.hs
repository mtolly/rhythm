{-# LANGUAGE BangPatterns #-}
{- |

Defines the three basic types for representing music game information:
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
import qualified Data.InfList as I

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

toBeats :: Resolution -> Ticks -> Beats
toBeats res tks = NN.fromNumberUnsafe $ fromIntegral tks / fromIntegral res

-- | Converts a rational duration to integer ticks. If converting multiple
-- consecutive durations, use 'toTickTrack' instead to avoid rounding error.
toTicks :: Resolution -> Beats -> Ticks
toTicks res bts = NN.fromNumberUnsafe $ floor $ bts * fromIntegral res

toBeatTrack :: Resolution -> RTB.T Ticks a -> RTB.T Beats a
toBeatTrack res = RTB.mapTime (toBeats res)

-- | Rounds each beat duration to an integral tick value. The rounding errors
-- are kept track of, and corrected for.
toTickTrack :: Resolution -> RTB.T Beats a -> RTB.T Ticks a
-- RTB.discretize ensures that rounding errors don't accumulate.
toTickTrack res = RTB.discretize .
  RTB.mapTime (* (NN.fromNumberUnsafe $ fromIntegral res))

-- | The smallest resolution needed to represent all times correctly.
minResolution :: [Beats] -> Resolution
minResolution = NN.fromNumberUnsafe . foldr (lcm . denominator . NN.toNumber) 1

-- | The smallest resolution needed to represent all event positions correctly.
minTrackResolution :: RTB.T Beats a -> Resolution
minTrackResolution = minResolution . RTB.getTimes

-- | Uses tempos to convert an event-list from beatstamps to timestamps. If no
-- tempo is present at time 0, 120 BPM is assumed.
beatsToTime :: (Ord a) => RTB.T Beats BPM -> RTB.T Beats a -> RTB.T Seconds a
beatsToTime bpms evts =
  go defBPM $ RTB.merge (RTB.mapBody Left bpms) (RTB.mapBody Right evts) where
    go :: (Ord a) => BPM -> RTB.T Beats (Either BPM a) -> RTB.T Seconds a
    go bpm xs = case RTB.viewL xs of
      Nothing -> RTB.empty
      Just ((db, x), rest) -> let dt = (db / bpm) * 60 in
        case x of
          Left bpm' -> RTB.delay dt $ go bpm' rest
          Right evt -> RTB.cons dt evt $ go bpm rest
    defBPM = 120 -- if no tempo at position 0, we assume 120 bpm.

-- | Uses tempos to convert an event-list from timestamps to beatstamps. If no
-- tempo is present at time 0, 120 BPM is assumed.
timeToBeats :: (Ord a) => RTB.T Seconds BPM -> RTB.T Seconds a -> RTB.T Beats a
timeToBeats bpms evts =
  go defBPM $ RTB.merge (RTB.mapBody Left bpms) (RTB.mapBody Right evts) where
    go :: (Ord a) => BPM -> RTB.T Seconds (Either BPM a) -> RTB.T Beats a
    go bpm xs = case RTB.viewL xs of
      Nothing -> RTB.empty
      Just ((dt, x), rest) -> let db = (dt / 60) * bpm in
        case x of
          Left bpm' -> RTB.delay db $ go bpm' rest
          Right evt -> RTB.cons db evt $ go bpm rest
    defBPM = 120 -- if no tempo at position 0, we assume 120 bpm.

-- | Each event-list is merged into a new list, starting at its position in the
-- original list. This is equivalent to the monad function join, but the Monad
-- typeclass can't be used because of typeclass constraints.
rtbJoin :: (NN.C t, Ord a) => RTB.T t (RTB.T t a) -> RTB.T t a
rtbJoin rtb = case RTB.viewL rtb of
  Nothing -> RTB.empty
  Just ((dt, x), rtb') -> RTB.delay dt $ RTB.merge x $ rtbJoin rtb'

-- | A time signature stored as a multiplier times a unit. The multiplier is the
-- same as the numerator in traditional notation, while the unit is the
-- reciprocal of the traditional denominator, times 4. For example, 4/4 time is
-- @TimeSignature 4 1@, while 6/8 time is @TimeSignature 6 0.5@.
data TimeSignature = TimeSignature
  { sigMultiplier :: NN.Integer -- ^ This many of the unit makes up a measure.
  , sigUnit :: Beats -- ^ The unit, as a fraction of quarter notes.
  } deriving (Eq, Ord, Show)

-- | A quantity of measures.
type Measures = NN.Integer
-- | A timeline of time signature changes.
type SignatureTrack = RTB.T Measures TimeSignature

-- | Note that both measure-position and beat-position start from 0. So in 4/4
-- time, what would traditionally be called Measure 2, Beat 3 is stored as
-- @MeasurePosn 1 2@.
data MeasurePosn = MeasurePosn
  { measures :: Measures
  -- ^ The number of full measures that has elapsed, starting from 0.
  , beats :: Beats
  -- ^ The number of beats from the beginning of a measure, starting from 0.
  } deriving (Eq, Ord, Show)

measureLength :: TimeSignature -> Beats
measureLength (TimeSignature mult unit) =
  unit * NN.fromNumberUnsafe (fromIntegral mult)

-- | The infinite list of measure lengths generated by time signatures.
measureLengths :: SignatureTrack -> I.InfList Beats
measureLengths = go defaultMeasureLength where
  go :: Beats -> SignatureTrack -> I.InfList Beats
  go cur sigs = case RTB.viewL sigs of
    Nothing -> I.repeat cur
    Just ((msrs, sig), rest) ->
      replicate (fromIntegral msrs) cur
        I.+++ go (measureLength sig) rest
  defaultMeasureLength = 4 -- If no timesig at position 0, we assume 4/4.

-- | Construct a time signature according to the traditional numerator &
-- denominator. For example, common time of 4 quarter notes per measure is
-- @4 // 4@, while 6 eighth notes is @6 // 8@.
(//) :: NN.Integer -> NN.Integer -> TimeSignature
num // denom = TimeSignature num $ 4 / fromIntegral denom
infix 5 // -- lower than (+) (-) (*) (/), same level as (:)

-- | Converts a position from beats to measures & beats.
getMeasurePosn :: SignatureTrack -> Beats -> MeasurePosn
getMeasurePosn sigs = go 0 $ measureLengths sigs where
  go :: Measures -> I.InfList Beats -> Beats -> MeasurePosn
  go !m (x I.::: xs) left = if left < x
    then MeasurePosn m left
    else go (m + 1) xs (left - x)

-- | Converts a position from measures & beats to just beats.
getBeatPosn :: SignatureTrack -> MeasurePosn -> Beats
getBeatPosn sigs (MeasurePosn m b) =
  b + sum (I.take (fromIntegral m) (measureLengths sigs))

-- | Drops any time signatures which are not on measure boundaries.
validSignatures :: RTB.T Beats TimeSignature -> SignatureTrack
validSignatures = go 4 where
  go :: Beats -> RTB.T Beats TimeSignature -> SignatureTrack
  go cur sigs = case RTB.viewL sigs of
    Just ((0, sig), rest) -> RTB.cons 0 sig $ go (measureLength sig) rest
    Just _ -> RTB.delay 1 $ go cur $ undelay cur sigs
    Nothing -> RTB.empty

renderSignatures :: SignatureTrack -> RTB.T Beats TimeSignature
renderSignatures = go 4 where
  go :: Beats -> SignatureTrack -> RTB.T Beats TimeSignature
  go cur sigs = case RTB.viewL sigs of
    Just ((msrs, sig), rest) ->
      RTB.cons (cur * fromIntegral msrs) sig $ go (measureLength sig) rest
    Nothing -> RTB.empty

-- | Drops the given amount of time from the event list. Events that are exactly
-- at the new beginning of the list will not be dropped.
undelay :: (NN.C t, Num t) => t -> RTB.T t a -> RTB.T t a
undelay t rtb = case RTB.viewL rtb of
  Just ((dt, x), rest) -> if dt < t
    then undelay (t - dt) rest
    else RTB.cons (dt - t) x rest
  Nothing -> RTB.empty
