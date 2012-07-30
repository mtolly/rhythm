{- |

The Event container stores events that have two different subtypes, for events
with duration and events that are just single points in time. There are two ways
you can use the Event type, depending on how it is parametrized.

  * A duration event can store a numeric type, the length of the event.
    The FeedBack .chart format works this way.

  * A duration event can store a Bool, which says whether it is the beginning
    (True) or end (False) of an event.

-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Data.Rhythm.Event where

import Data.Rhythm.Time
import qualified Numeric.NonNegative.Class as NN
import qualified Data.EventList.Relative.TimeBody as RTB
import Control.Monad (guard, (>=>))

-- | A class for types which are split into two subtypes: those that store
-- \"long\" events (which have duration) and those that store \"point\" events
-- (with zero duration).
class (Ord l, Ord p) => Duration l p | l -> p where
  -- | If the two events can form an on/off pair, returns a single value that
  -- represents the whole event.
  condense :: l -> l -> Maybe l
  condense x y = guard (x == y) >> Just x

data Event l p t
  = Long t l -- ^ An event that has start and end points.
  | Point p -- ^ An event located at a single point in time.
  deriving (Eq, Ord, Show, Read)

instance Functor (Event l p) where
  fmap g (Long t l) = Long (g t) l
  fmap _ (Point p) = Point p

-- | Convert from events that store a length to separate on/off events. Each
-- duration-event is split into an on-event and an off-event, both with the same
-- value as the old duration event.
lengthToSwitch :: (NN.C t, Duration l p) =>
  RTB.T t (Event l p t) -> RTB.T t (Event l p Bool)
lengthToSwitch = rtbJoin . fmap f where
  f (Point x) = RTB.singleton NN.zero (Point x)
  f (Long dt x) = RTB.fromPairList
    [(NN.zero, Long True x), (dt, Long False x)]

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
switchToLength :: (NN.C t, Duration l p, Num t) =>
  RTB.T t (Event l p Bool) -> RTB.T t (Event l p t)
switchToLength rtb = case RTB.viewL rtb of
  Nothing -> RTB.empty
  Just ((dt, Point p), rest) -> RTB.cons dt (Point p) $ switchToLength rest
  Just ((dt, Long False _), rest) -> RTB.delay dt $ switchToLength rest
    -- An end with no start before it is dropped.
  Just ((dt, Long True x), rest) ->
    case extractFirst (isEnd >=> condense x) rest of
      Nothing -> RTB.delay dt $ switchToLength rest
        -- A start with no end after it is dropped.
      Just ((pos, y), rest') -> RTB.cons dt (Long pos y) $ switchToLength rest'
      where isEnd (Long False l) = Just l; isEnd _ = Nothing
