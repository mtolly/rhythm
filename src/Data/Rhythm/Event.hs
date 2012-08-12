{- |

The Event container stores events that have two different subtypes, for events
with duration and events that are just single points in time. There are two ways
you can use the Event type, depending on how it is parametrized.

  * A duration event can store a numeric type, the length of the event.
    The FeedBack .chart format works this way.

  * A duration event can store a Bool, which says whether it is the beginning
    (True) or end (False) of an event.

-}
module Data.Rhythm.Event where

import Data.Rhythm.Time
import qualified Numeric.NonNegative.Class as NN
import qualified Data.EventList.Relative.TimeBody as RTB
import Control.Monad (guard, (>=>))

-- | Class for events which store an event over some duration of time. Such an
-- event can be stored as an on/off pair, or a single event.
class (Ord a) => Long a where
  -- | True if the two events form an on/off pair (first argument is on, second
  -- is off). Default implementation is @(==)@.
  match :: a -> a -> Bool
  match x y = x == y
  -- | Returns a single event representing the on/off pair (first argument is
  -- on, second is off). Default implementation uses 'match' and then returns
  -- the first argument, so you can usually just override 'match'.
  condense :: a -> a -> Maybe a
  condense x y = guard (match x y) >> Just x

data Event l p t
  = Length t l -- ^ An event that has start and end points.
  | Point p -- ^ An event located at a single point in time.
  deriving (Eq, Ord, Show, Read)

instance Functor (Event l p) where
  fmap g (Length t l) = Length (g t) l
  fmap _ (Point p) = Point p

-- | Convert from events that store a length to separate on/off events. Each
-- duration-event is split into an on-event and an off-event, both with the same
-- value as the old duration event.
lengthToSwitch :: (NN.C t, Long l, Ord p) =>
  RTB.T t (Event l p t) -> RTB.T t (Event l p Bool)
lengthToSwitch = rtbJoin . fmap f where
  f (Point x) = RTB.singleton NN.zero (Point x)
  f (Length dt x) = RTB.fromPairList
    [(NN.zero, Length True x), (dt, Length False x)]

-- | The first event for which the function gives a Just result is removed
-- from the list, along with its position.
extractFirst :: (NN.C t, Num t) =>
  (a -> Maybe b) -> RTB.T t a -> Maybe ((t, b), RTB.T t a)
extractFirst f rtb = RTB.viewL rtb >>= \((dt, x), rest) -> case f x of
  Just y -> Just ((dt, y), RTB.delay dt rest)
  Nothing -> extractFirst f rest >>= \((pos, y), rest') ->
    Just ((dt + pos, y), RTB.cons dt x rest')

-- | Converts from separate on/off events to events that store a length. An
-- on-event and off-event will be joined according to the 'condense' method of
-- the 'Long' class.
switchToLength :: (NN.C t, Long l, Ord p, Num t) =>
  RTB.T t (Event l p Bool) -> RTB.T t (Event l p t)
switchToLength rtb = case RTB.viewL rtb of
  Nothing -> RTB.empty
  Just ((dt, Point p), rest) -> RTB.cons dt (Point p) $ switchToLength rest
  Just ((dt, Length False _), rest) -> RTB.delay dt $ switchToLength rest
    -- An end with no start before it is dropped.
  Just ((dt, Length True x), rest) ->
    case extractFirst (isEnd >=> condense x) rest of
      Nothing -> RTB.delay dt $ switchToLength rest
        -- A start with no end after it is dropped.
      Just ((p, y), rest') -> RTB.cons dt (Length p y) $ switchToLength rest'
      where isEnd (Length False l) = Just l; isEnd _ = Nothing

toBeatTrack' :: Functor f =>
  Resolution -> RTB.T Ticks (f Ticks) -> RTB.T Beats (f Beats)
toBeatTrack' res = toBeatTrack res . fmap (fmap $ toBeats res)

toTickTrack' :: Functor f =>
  Resolution -> RTB.T Beats (f Beats) -> RTB.T Ticks (f Ticks)
toTickTrack' res = toTickTrack res . fmap (fmap $ toTicks res)

-- | The smallest resolution needed to represent all event positions and
-- durations correctly.
minTrackResolution' :: RTB.T Beats (Event l p Beats) -> Resolution
minTrackResolution' rtb = minResolution $
  RTB.getTimes rtb ++ [len | Length len _ <- RTB.getBodies rtb]
