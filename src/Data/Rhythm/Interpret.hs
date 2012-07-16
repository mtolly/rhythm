{- |
The rhythm library is built around data formats which can be viewed at one of
several layers of structure, and which often have only partial public
documentation. This module provides a common interface to the task of parsing
a general format into a more structured one, while possibly generating warnings
or errors.
-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Rhythm.Interpret where

import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Numeric.NonNegative.Class as NNC

class Interpret a b where
  interpret :: a -> Maybe ([b], [String])

interpretEvents :: (Interpret a b, NNC.C t) =>
  RTB.T t a -> (RTB.T t b, RTB.T t String, RTB.T t a)
interpretEvents evts = case RTB.partitionMaybe interpret evts of
  (out, bad) -> (good, warns, bad) where
    good  = RTB.flatten $ fmap fst out
    warns = RTB.flatten $ fmap snd out

ok :: a -> Maybe ([a], [b])
ok x = okList [x]

okList :: [a] -> Maybe ([a], [b])
okList xs = Just (xs, [])
