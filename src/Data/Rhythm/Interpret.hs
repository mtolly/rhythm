{- |
A common interface for parsing from a general type to a specific one, and
vice versa.
-}
module Data.Rhythm.Interpret where

import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Numeric.NonNegative.Class as NN
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class

-- | General, raw type -> specific, strict type. Unrecognized input returns
-- Nothing. Recognized input can also generate warnings using the Writer monad.
type Interpreter a b = a -> WriterT [String] Maybe [b]

interpretRTB :: (NN.C t) => Interpreter a b ->
  RTB.T t a -> (RTB.T t b, RTB.T t String, RTB.T t a)
interpretRTB f rtb = case RTB.partitionMaybe (runWriterT . f) rtb of
  (out, bad) -> (good, warns, bad) where
    good  = RTB.flatten $ fmap fst out
    warns = RTB.flatten $ fmap snd out

warn :: (Monad m) => w -> WriterT [w] m ()
warn x = tell [x]

single :: a -> WriterT [w] Maybe [a]
single x = return [x]

none :: WriterT [w] Maybe a
none = lift Nothing

-- | Specific, strict type -> general, raw type. No errors/warnings because
-- the conversion always succeeds.
type Uninterpreter a b = a -> [b]

uninterpretRTB :: (NN.C t) => Uninterpreter a b -> RTB.T t a -> RTB.T t b
uninterpretRTB f = RTB.flatten . fmap f
