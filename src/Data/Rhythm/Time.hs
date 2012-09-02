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
import qualified Data.Rhythm.Status as Status

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

applyTime :: (NN.C t, NN.C u) => Status.T t (t -> u) -> RTB.T t a -> RTB.T u a
applyTime (Status.Stay f) rtb = RTB.mapTime f rtb
applyTime (Status.For t f fs) rtb = case RTB.viewL rtb of
  Nothing -> RTB.empty
  Just ((u, x), xs) -> case NN.split t u of
    (m, (b, d)) -> if b
      then {- t <= u -} RTB.delay (f m) $ applyTime fs $ RTB.cons d x xs
      else {- t > u -} RTB.cons (f m) x $ applyTime (Status.For d f fs) xs

-- | Uses tempos to convert an event-list from beatstamps to timestamps.
toTimeTrack :: Status.T Beats BPM -> RTB.T Beats a -> RTB.T Seconds a
toTimeTrack = applyTime . fmap toTime

-- | Uses tempos to convert an event-list from timestamps to beatstamps.
fromTimeTrack :: Status.T Seconds BPM -> RTB.T Seconds a -> RTB.T Beats a
fromTimeTrack = applyTime . fmap fromTime

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
type SignatureTrack = Status.T Measures TimeSignature

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
measureLengths (Status.Stay sig) = I.repeat $ measureLength sig
measureLengths (Status.For msrs sig rest) =
  replicate (fromIntegral msrs) (measureLength sig) I.+++ measureLengths rest

-- | Construct a time signature according to the traditional numerator &
-- denominator. For example, common time of 4 quarter notes per measure is
-- @4 // 4@, while 6 eighth notes is @6 // 8@.
(//) :: NN.Integer -> NN.Integer -> TimeSignature
num // denom = TimeSignature num $ 4 / fromIntegral denom
infix 5 // -- lower than (+) (-) (*) (/), same level as (:)

-- | Converts a position from beats to measures & beats.
getMeasurePosn :: SignatureTrack -> Beats -> MeasurePosn
getMeasurePosn = go 0 . measureLengths where
  go :: Measures -> I.InfList Beats -> Beats -> MeasurePosn
  go !m (x I.::: xs) bts = case NN.split bts x of
    (_, (False, _)) {- bts < x -} -> MeasurePosn m bts
    (_, (True, d)) {- bts >= x -} -> go (succ m) xs d

-- | Converts a position from measures & beats to just beats.
getBeatPosn :: SignatureTrack -> MeasurePosn -> Beats
getBeatPosn sigs (MeasurePosn m b) =
  b + sum (I.take (fromIntegral m) (measureLengths sigs))

naturalSignature :: Beats -> TimeSignature
naturalSignature bts = case NN.toNumber bts of
  rat -> TimeSignature (NN.fromNumberUnsafe n) (NN.fromNumberUnsafe $ 1 % d)
    where (n, d) = (numerator rat, denominator rat)

-- | A time signature in the middle of a measure ends it.
validSignatures :: Status.T Beats TimeSignature -> SignatureTrack
validSignatures (Status.Stay sig) = Status.Stay sig
validSignatures (Status.For bts sig rest) = case properFraction $ bts / mlen of
  (w, 0) -> Status.For (NN.fromNumberUnsafe w) sig $ validSignatures rest
  (w, f) -> Status.For (NN.fromNumberUnsafe w) sig $
    Status.For 1 (naturalSignature $ f * mlen) $ validSignatures rest
  where mlen = measureLength sig :: Beats

renderSignatures :: SignatureTrack -> Status.T Beats TimeSignature
renderSignatures (Status.Stay sig) = Status.Stay sig
renderSignatures (Status.For msrs sig rest) =
  Status.For (measureLength sig * fromIntegral msrs) sig $ renderSignatures rest
