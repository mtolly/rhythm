{- | Utility functions for Data.EventList.Relative.TimeBody. -}
module Data.Rhythm.EventList where

import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Numeric.NonNegative.Class as NN
import Data.Traversable (traverse)
import Control.Applicative (Applicative)
import Control.Monad (guard)
import Prelude hiding (take)

filterA :: (NN.C t, Applicative f) =>
  (a -> f Bool) -> RTB.T t a -> f (RTB.T t a)
filterA g = fmap (RTB.mapMaybe ifSnd) . traverse attachG where
  ifSnd (x, b) = guard b >> Just x
  attachG x = fmap (\b -> (x, b)) $ g x

-- | Each event-list is merged into a new list, starting at its position in the
-- original list. This is equivalent to Control.Monad.join, but the Monad
-- typeclass can't be used because of typeclass constraints.
join :: (NN.C t, Ord a) => RTB.T t (RTB.T t a) -> RTB.T t a
join rtb = case RTB.viewL rtb of
  Nothing -> RTB.empty
  Just ((dt, x), rtb') -> RTB.delay dt $ RTB.merge x $ join rtb'

-- | Drops all events after the specified amount of time, including events
-- exactly on the edge.
take :: (NN.C t, Ord a) => t -> RTB.T t a -> RTB.T t a
take t rtb = case RTB.viewL rtb of
  Nothing -> rtb
  Just ((dt, x), rtb') -> case NN.split t dt of
    (_, (True, _)) {- t <= dt -} -> RTB.empty
    (_, (False, d)) {- t > dt -} -> RTB.cons dt x $ take d rtb'
