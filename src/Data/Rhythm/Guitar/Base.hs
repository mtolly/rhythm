module Data.Rhythm.Guitar.Base where

type GtrFret = Int

data SixString = S6 | S5 | S4 | S3 | S2 | S1
	deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | A tuning is a list of string pitches, from lowest to highest.
type Tuning a = [a]

play :: (Num a) => SixString -> GtrFret -> Tuning a -> a
play s f t = (t !! fromEnum s) + fromIntegral f

stdTuning :: (Num a) => Tuning a
stdTuning = [40, 45, 50, 55, 59, 64]

-- | Converts from absolute pitches to standard tuning offsets.
toOffsets :: (Num a) => Tuning a -> Tuning a
toOffsets = zipWith subtract stdTuning

-- | Converts from standard tuning offsets to absolute pitches.
fromOffsets :: (Num a) => Tuning a -> Tuning a
fromOffsets = zipWith (+) stdTuning

dropD :: (Num a) => Tuning a
dropD = fromOffsets [-2, 0, 0, 0, 0, 0]

