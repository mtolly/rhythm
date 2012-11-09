module Data.Rhythm.Guitar where

import qualified Numeric.NonNegative.Wrapper as NN

type GtrFret = NN.Int

data SixString = S6 | S5 | S4 | S3 | S2 | S1
	deriving (Eq, Ord, Show, Read, Enum, Bounded)

type SixTuning a = SixString -> a

play :: (Num a) => SixString -> GtrFret -> SixTuning a -> a
play s f t = t s + fromIntegral f

-- | The MIDI pitches of standard (EADGBE) guitar tuning.
stdTuning :: (Num a) => SixTuning a
stdTuning s = [40, 45, 50, 55, 59, 64] !! fromEnum s

-- | The MIDI pitches of drop D (DADGBE) guitar tuning.
dropD :: (Num a) => SixTuning a
dropD S6 = 38
dropD s = stdTuning s
