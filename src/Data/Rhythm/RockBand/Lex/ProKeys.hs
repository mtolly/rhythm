{-# LANGUAGE ViewPatterns, MultiParamTypeClasses,
    TypeSynonymInstances, FlexibleInstances #-}
-- | The contents of the \"PART REAL_KEYS_?\" and \"KEYS_ANIM_?H\" tracks.
module Data.Rhythm.RockBand.Lex.ProKeys where

import Data.Rhythm.RockBand.Common
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Data.Rhythm.RockBand.Lex.MIDI as MIDI
import Data.Rhythm.Event
import Data.Rhythm.Time
import qualified Numeric.NonNegative.Class as NNC
import Data.Rhythm.Interpret

instance Duration Long Point
type T = Event Long Point

data Point
  {- | Change the viewable play range. Should be placed at least a measure
       before any notes that require the new range. -}
  = LaneShift LaneRange
  -- | The beginning/end of Pro Keys trainer sections.
  | Trainer Trainer
  | Mood Mood
  deriving (Eq, Ord, Show)

data Long
  = Solo -- ^ A keyboard solo section.
  | Glissando -- ^ Place over a sequence of white notes for a freeform section.
  | Trill -- ^ Fill lanes on two keys.
  | Overdrive -- ^ An energy phrase.
  | BRE -- ^ Fill lanes for a Big Rock Ending.
  | Note V.Pitch -- ^ Valid pitches are in MIDI range 48 to 72.
  deriving (Eq, Ord, Show)

-- | There are six playable ranges, each of which covers 10 white keys, plus
-- all the black keys within. They are named here according to their lowest key.
data LaneRange = C | D | E | F | G | A
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance (NNC.C a) => Interpret (MIDI.T a) (T a) where
  interpret (Long len (MIDI.Note _ p _)) = case V.fromPitch p of
    0 -> ok $ Point $ LaneShift C
    2 -> ok $ Point $ LaneShift D
    4 -> ok $ Point $ LaneShift E
    5 -> ok $ Point $ LaneShift F
    7 -> ok $ Point $ LaneShift G
    9 -> ok $ Point $ LaneShift A
    i | 48 <= i && i <= 72 -> ok $ Long len $ Note p
    115 -> ok $ Long len Solo
    116 -> ok $ Long len Overdrive
    120 -> ok $ Long len BRE
    126 -> ok $ Long len Glissando
    127 -> ok $ Long len Trill
    _ -> Nothing
  interpret (Point (MIDI.TextEvent str)) = case str of
    (readMood -> Just m) -> ok $ Point $ Mood m
    (readTrainer -> Just (t, "key")) -> ok $ Point $ Trainer t
    _ -> Nothing
  interpret _ = Nothing

instance Interpret (T Beats) (MIDI.T Beats) where
  interpret (Point p) = ok $ case p of
    LaneShift rng -> MIDI.blip $ V.toPitch $ [0, 2, 4, 5, 7, 9] !! fromEnum rng
    Trainer t -> Point $ MIDI.TextEvent $ showTrainer t "key"
    Mood m -> Point $ MIDI.TextEvent $ showMood m
  interpret (Long len l) = ok $ Long len $ MIDI.standardNote $ case l of
    Solo -> V.toPitch 115
    Glissando -> V.toPitch 126
    Trill -> V.toPitch 127
    Overdrive -> V.toPitch 116
    BRE -> V.toPitch 120
    Note p -> p
