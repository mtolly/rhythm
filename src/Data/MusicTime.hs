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

Also defined is the TimeEvent container, for events which have different types
depending on whether they have a duration. This container can be used in two
ways.

  * The duration type can be a numeric type, and a numeric duration is then
    stored in the event. This corresponds to how the FeedBack .chart format is
    designed.

  * The duration type can be Bool, and then a duration event is actually one of
    two events, an on-event (with a True duration) and an off-event (with a
    False duration), which together denote the boundaries of an event.

-}
module Data.MusicTime where

import qualified Numeric.NonNegative.Wrapper as NN
import qualified Numeric.NonNegative.Class as NNC
import qualified Data.EventList.Relative.TimeBody as RTB
import Data.Ratio

-- | A beat, or quarter note, is the most general timekeeping unit.
type Beats = NN.Rational
type BeatTrack = RTB.T Beats
-- | Use this if the event type also has some kind of duration inside it.
type BeatTrack' f = BeatTrack (f Beats)

{- | In file formats such as MIDI, beats are encoded as numerators with a fixed
     denominator (the 'Resolution'). -}
type Ticks = NN.Integer
{- | Defines how many ticks are in one beat. -}
type Resolution = Ticks
type TickTrack = RTB.T Ticks
type TickTrack' f = TickTrack (f Ticks)

-- | A precise time position/duration in seconds.
type Seconds = NN.Rational
type TimeTrack = RTB.T Seconds
type TimeTrack' f = TimeTrack (f Seconds)

-- | A tempo, in beats per minute.
type BPM = NN.Rational

toBeats :: Resolution -> Ticks -> Beats
toBeats res tks = NN.fromNumberUnsafe $ fromIntegral tks / fromIntegral res

{- | Converts a rational duration to integer ticks. If converting multiple
     consecutive durations, use tickTrack instead to avoid rounding error. -}
toTicks :: Resolution -> Beats -> Ticks
toTicks res bts = NN.fromNumberUnsafe $ floor $ bts * fromIntegral res

beatTrack :: Resolution -> TickTrack a -> BeatTrack a
beatTrack res = RTB.mapTime (toBeats res)

{- | Rounds each beat duration to an integral tick value. The rounding errors
     are kept track of, and corrected. -}
tickTrack :: Resolution -> BeatTrack a -> TickTrack a
-- RTB.discretize ensures that rounding errors don't accumulate.
tickTrack res = RTB.discretize .
  RTB.mapTime (* (NN.fromNumberUnsafe $ fromIntegral res))

-- | Returns the smallest resolution needed to represent all times correctly.
minResolution :: BeatTrack a -> Resolution
minResolution = NN.fromNumberUnsafe . foldr lcm 1 .
  map (denominator . NN.toNumber) . RTB.getTimes

{- | Uses tempos to convert an event-list from beatstamps to timestamps.
     If no tempo is present at time 0, 120 BPM is assumed. -}
beatsToTime :: (Ord a) => BeatTrack BPM -> BeatTrack a -> TimeTrack a
beatsToTime bpms evts =
  go 120 $ RTB.merge (RTB.mapBody Left bpms) (RTB.mapBody Right evts) where
    go :: (Ord a) => BPM -> BeatTrack (Either BPM a) -> TimeTrack a
    go bpm xs = case RTB.viewL xs of
      Nothing -> RTB.empty
      Just ((db, x), rest) -> let dt = (db / bpm) * 60 in
        case x of
          Left bpm' -> RTB.delay dt $ go bpm' rest
          Right evt -> RTB.cons dt evt $ go bpm rest

{- | Uses tempos to convert an event-list from timestamps to beatstamps.
     If no tempo is present at time 0, 120 BPM is assumed. -}
timeToBeats :: (Ord a) => TimeTrack BPM -> TimeTrack a -> BeatTrack a
timeToBeats bpms evts =
  go 120 $ RTB.merge (RTB.mapBody Left bpms) (RTB.mapBody Right evts) where
    go :: (Ord a) => BPM -> TimeTrack (Either BPM a) -> BeatTrack a
    go bpm xs = case RTB.viewL xs of
      Nothing -> RTB.empty
      Just ((dt, x), rest) -> let db = (dt / 60) * bpm in
        case x of
          Left bpm' -> RTB.delay db $ go bpm' rest
          Right evt -> RTB.cons db evt $ go bpm rest

{- | Each event-list is merged into a new list, starting at its position in the
     original list. This is equivalent to the monad function join, but the Monad
     typeclass can't be used because of typeclass constraints. -}
rtbJoin :: (NNC.C t, Ord a) => RTB.T t (RTB.T t a) -> RTB.T t a
rtbJoin rtb = case RTB.viewL rtb of
  Nothing -> RTB.empty
  Just ((t, b), rtb') -> RTB.delay t $ RTB.merge b $ rtbJoin rtb'

-- | A container for events that can either have duration or not.
data TimeEvent d p a
  = Duration d a
  | Point p
  -- two useful Ord properties:
  --   duration sorts before point, e.g. tom markers sort before drum notes.
  --   when a is Bool, off-events (False) sort before on-events (True).
  deriving (Eq, Ord, Show, Read)

type DurationTrack d p t = RTB.T t (TimeEvent d p t)
type SwitchTrack d p t = RTB.T t (TimeEvent d p Bool)

instance Functor (TimeEvent d p) where
  fmap f (Duration x t) = Duration x (f t)
  fmap _ (Point p) = Point p

{- | Convert from events that store a duration to separate on/off events. Each
     duration-event is split into an on-event and an off-event, both with the
     same value as the old duration event. -}
durationToSwitch :: (NNC.C t, Ord d, Ord p) =>
  DurationTrack d p t -> SwitchTrack d p t
durationToSwitch = rtbJoin . fmap f where
  f (Point x) = RTB.singleton NNC.zero (Point x)
  f (Duration x dt) =
    RTB.fromPairList [(NNC.zero, Duration x True), (dt, Duration x False)]

{- | Converts from separate on/off events to events that store a duration. The
     first argument is the function for determining whether two duration-events
     are linked together; if so, the off-event is removed and the on-event is
     changed to hold the duration between the on-event and off-event. -}
switchToDuration :: (NNC.C t) => (d -> d -> Bool) ->
  SwitchTrack d p t -> DurationTrack d p t
-- If an on event is followed by another on event, we should probably behave
-- as if there's an off event simultaneous with the second on event.
-- An off event not preceded by an on event should do nothing.
-- An on event not followed by an on/off event should also do nothing.
switchToDuration = undefined

-- | A time signature stored as a multiplier times a unit. The multiplier is the
-- same as the numerator in traditional notation, while the unit is the
-- reciprocal of the traditional denominator, times 4. For example, 4/4 time is
-- @TimeSignature 4 1@, while 6/8 time is @TimeSignature 6 0.5@.
data TimeSignature = TimeSignature
  { sigMultiplier :: NN.Integer -- ^ This many of the unit makes up a measure.
  , sigUnit :: Beats -- ^ The unit, as a fraction of quarter notes.
  } deriving (Eq, Ord, Show)

measureLength :: TimeSignature -> Beats
measureLength (TimeSignature mult unit) =
  unit * NN.fromNumberUnsafe (fromIntegral mult)

type Measures = NN.Integer
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

-- | An infinite list of measure lengths generated by time signatures.
measureLengths :: SignatureTrack -> [Beats]
measureLengths sigs = repeatLast eachMeasure 4 where
  pairs = RTB.toPairList sigs :: [(Measures, TimeSignature)]
  deltaTimes = map (fromIntegral . fst) pairs ++ [1] :: [Int]
  newLengths = 4 : map (measureLength . snd) pairs :: [Beats]
  eachMeasure = concat $ zipWith replicate deltaTimes newLengths
  
  -- Converts a list into an infinite list by repeating the last element, or
  -- the given element if it is an empty list.
  repeatLast :: [a] -> a -> [a]
  repeatLast []     x = repeat x
  repeatLast (x:xs) _ = x : repeatLast xs x

-- | Construct a time signature according to the traditional numerator &
-- denominator. For example, common time of 4 quarter notes per measure is
-- @4 // 4@, while 6 eighth notes is @6 // 8@.
(//) :: NN.Integer -> NN.Integer -> TimeSignature
num // denom = TimeSignature num $ recip $ fromIntegral denom / 4
infix 5 // -- lower than (+) (-) (*) (/), same level as (:)

-- | Converts a position from beats to measures & beats.
getMeasurePosn :: SignatureTrack -> Beats -> MeasurePosn
getMeasurePosn sigs bts = MeasurePosn m b where
  thresholds = takeWhile (<= bts) $ sums $ measureLengths sigs
  m = fromIntegral $ length thresholds
  b = bts - case reverse thresholds of (x:_) -> x; _ -> 0
  
  -- For [a,b,c], returns [a, a+b, a+b+c].
  sums :: (Num a) => [a] -> [a]
  sums = go 0 where
    go _ [] = []
    go !n (x:xs) = let nx = n + x in nx : go nx xs
    -- bang pattern ensures constant space usage

-- | Converts a position from measures & beats to just beats.
getBeatPosn :: SignatureTrack -> MeasurePosn -> Beats
getBeatPosn sigs (MeasurePosn m b) =
  b + sum (take (fromIntegral m) (measureLengths sigs))
