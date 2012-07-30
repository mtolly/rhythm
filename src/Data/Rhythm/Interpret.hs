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
import qualified Numeric.NonNegative.Class as NN
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class

class Interpret a b where
  interpret :: a -> WriterT [String] Maybe [b]

interpretEvents :: (Interpret a b, NN.C t) =>
  RTB.T t a -> (RTB.T t b, RTB.T t String, RTB.T t a)
interpretEvents evts = case RTB.partitionMaybe (runWriterT . interpret) evts of
  (out, bad) -> (good, warns, bad) where
    good  = RTB.flatten $ fmap fst out
    warns = RTB.flatten $ fmap snd out

warn :: (Monad m) => w -> WriterT [w] m ()
warn x = tell [x]

single :: a -> WriterT [w] Maybe [a]
single x = return [x]

none :: WriterT [w] Maybe a
none = lift Nothing
