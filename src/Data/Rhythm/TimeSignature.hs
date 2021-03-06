{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}
module Data.Rhythm.TimeSignature where

import qualified Numeric.NonNegative.Wrapper as NN
import qualified Numeric.NonNegative.Class as NN
import Data.Ratio
import qualified Data.InfList as I
import qualified Data.Rhythm.Status as Status
import Data.Rhythm.Time
import Data.Monoid (Monoid)

-- | A time signature stored as a multiplier times a unit. The multiplier is the
-- same as the numerator in traditional notation, while the unit is the
-- reciprocal of the traditional denominator, times 4. For example, 4/4 time is
-- @TimeSignature 4 1@, while 6/8 time is @TimeSignature 6 0.5@.
data TimeSignature = TimeSignature
  { sigMultiplier :: NN.Integer -- ^ This many of the unit makes up a measure.
  , sigUnit :: Beats -- ^ The unit, as a fraction of quarter notes.
  } deriving (Eq, Ord, Show)

-- | A quantity of measures.
newtype Measures = Measures { unMeasures :: NN.Integer }
  deriving (Eq, Ord, Show, Enum, Num, Real, Integral, NN.C, Monoid)
-- | A timeline of time signature changes.
type SignatureTrack = Status.T Measures TimeSignature

-- | Note that both measure-position and beat-position start from 0. So in 4/4
-- time, what would traditionally be called Measure 2, Beat 3 is stored as
-- @MeasurePosn 1 2@.
data MeasurePosn = MeasurePosn
  { measures :: Measures
  -- ^ The number of full measures that has elapsed, starting from 0.
  , beats :: Beats
  -- ^ The number of beats from the beginning of a measure, starting from 0.
  } deriving (Eq, Ord, Show)

measureLength :: TimeSignature -> Beats
measureLength (TimeSignature mult (Beats unit)) =
  Beats $ unit * NN.fromNumberUnsafe (fromIntegral mult)

measureSignatures :: SignatureTrack -> I.InfList TimeSignature
measureSignatures (Status.Stay sig) = I.repeat sig
measureSignatures (Status.For msrs sig rest) =
  replicate (fromIntegral msrs) sig I.+++ measureSignatures rest

-- | The infinite list of measure lengths generated by time signatures.
measureLengths :: SignatureTrack -> I.InfList Beats
measureLengths = fmap measureLength . measureSignatures

-- | Construct a time signature according to the traditional numerator &
-- denominator. For example, common time of 4 quarter notes per measure is
-- @4 // 4@, while 6 eighth notes is @6 // 8@.
(//) :: NN.Integer -> NN.Integer -> TimeSignature
num // denom = TimeSignature num $ Beats $ 4 / fromIntegral denom
infix 5 // -- lower than (+) (-) (*) (/), same level as (:)

-- | Converts a position from beats to measures & beats.
getMeasurePosn :: SignatureTrack -> Beats -> MeasurePosn
getMeasurePosn = go 0 . measureLengths where
  go :: Measures -> I.InfList Beats -> Beats -> MeasurePosn
  go !m (x I.::: xs) bts = case NN.split bts x of
    (_, (False, _)) {- bts < x -} -> MeasurePosn m bts
    (_, (True, d)) {- bts >= x -} -> go (succ m) xs d

-- | Converts a position from measures & beats to just beats.
getBeatPosn :: SignatureTrack -> MeasurePosn -> Beats
getBeatPosn sigs (MeasurePosn m b) =
  b + sum (I.take (fromIntegral m) (measureLengths sigs))

naturalSignature :: Beats -> TimeSignature
naturalSignature (Beats bts) = let
  rat = NN.toNumber bts
  (n, d) = (numerator rat, denominator rat)
  in TimeSignature (NN.fromNumberUnsafe n) (Beats $ NN.fromNumberUnsafe $ 1 % d)

-- | A time signature in the middle of a measure ends it.
validSignatures :: Status.T Beats TimeSignature -> SignatureTrack
validSignatures (Status.Stay sig) = Status.Stay sig
validSignatures (Status.For bts sig rest) = case properFraction $ bts / mlen of
  (w, 0) -> Status.For w sig $ validSignatures rest
  (w, f) -> Status.For w sig $
    Status.For 1 (naturalSignature $ f * mlen) $ validSignatures rest
  where mlen = measureLength sig :: Beats

renderSignatures :: SignatureTrack -> Status.T Beats TimeSignature
renderSignatures (Status.Stay sig) = Status.Stay sig
renderSignatures (Status.For msrs sig rest) =
  Status.For (measureLength sig * fromIntegral msrs) sig $ renderSignatures rest
