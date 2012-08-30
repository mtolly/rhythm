-- | A data strcture similar to 'Data.EventList.Relative.TimeBody.T', but which
-- defines a status which has some value at any point in time.
module Data.Rhythm.Status where

import qualified Numeric.NonNegative.Class as NN
import qualified Data.EventList.Relative.TimeBody as RTB

-- | Functionally this is equivalent to 'TimeBody.T' with a guaranteed event at
-- position zero.
data Status t a = Stay a | For t a (Status t a)
  deriving (Eq, Ord, Show, Read)

-- | Removes zero-duration and redundant statuses.
clean :: (NN.C t, Eq a) => Status t a -> Status t a
clean = cleanRedundant . cleanZero

-- | Removes zero-duration statuses.
cleanZero :: (NN.C t) => Status t a -> Status t a
cleanZero (Stay x) = Stay x
cleanZero (For t x rest) = if t == NN.zero
  then cleanZero rest
  else For t x $ cleanZero rest

-- | Combines any two adjacent equal statuses.
cleanRedundant :: (NN.C t, Eq a) => Status t a -> Status t a
cleanRedundant (Stay x) = Stay x
cleanRedundant f@(For t x xs) = case xs of
  Stay y -> if x == y
    then Stay x
    else f
  For u y ys -> if x == y
    then cleanRedundant $ For (NN.add t u) x ys
    else For t x $ cleanRedundant xs

-- | Combines an initial value and an event-list of changes.
fromRTB :: a -> RTB.T t a -> Status t a
fromRTB x rtb = case RTB.viewL rtb of
  Nothing -> Stay x
  Just ((dt, y), rtb') -> For dt x $ fromRTB y rtb'

-- | Generates an event-list of changes, with a guaranteed event at position 0.
toRTB :: (NN.C t) => Status t a -> RTB.T t a
toRTB (Stay x) = RTB.singleton NN.zero x
toRTB (For dt x rest) = RTB.cons NN.zero x $ RTB.delay dt $ toRTB rest
