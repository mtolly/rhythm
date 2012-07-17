{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
-- | The contents of the \"BEAT\" track.
module Data.Rhythm.RockBand.Lex.Beat where

import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Data.Rhythm.RockBand.Lex.MIDI as MIDI
import Data.Rhythm.Event
import Data.Rhythm.Time
import qualified Numeric.NonNegative.Class as NNC
import Data.Rhythm.Interpret

data T
  = Bar -- ^ A thick barline; the beginning of a new measure.
  | Beat -- ^ A thin barline; a beat in the middle of a measure.
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance (NNC.C a) => Interpret (MIDI.T a) T where
  interpret (Long _ (MIDI.Note _ p _)) = case V.fromPitch p of
    12 -> single Bar
    13 -> single Beat
    _ -> none
  interpret _ = none

instance Interpret T (MIDI.T Beats) where
  interpret b = single $ MIDI.blip $ V.toPitch $ case b of Bar -> 12; Beat -> 13
