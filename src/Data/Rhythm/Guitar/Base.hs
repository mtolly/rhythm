module Data.Rhythm.Guitar.Base where

type GtrFret = Int

data SixString = S6 | S5 | S4 | S3 | S2 | S1
	deriving (Eq, Ord, Show, Read, Enum, Bounded)

type Tuning s a = s -> a

stdTuning :: (Num a) => Tuning SixString a
stdTuning = ([40, 45, 50, 55, 59, 64] !!) . fromEnum

dropD :: (Num a) => Tuning SixString a
dropD S6 = stdTuning S6 - 2
dropD str = stdTuning str

-- | Tunes all the strings up or down by some amount.
offset :: (Num a) => a -> Tuning s a -> Tuning s a
offset off tun = (+ off) . tun

-- | The note produced by a given string, fret, and tuning.
play :: (Num a) => s -> GtrFret -> Tuning s a -> a
play str frt tun = tun str + fromIntegral frt

