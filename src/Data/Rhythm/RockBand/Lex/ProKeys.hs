{-# LANGUAGE ViewPatterns, MultiParamTypeClasses #-}
-- | The contents of the \"PART REAL_KEYS_?\" and \"KEYS_ANIM_?H\" tracks.
module Data.Rhythm.RockBand.Lex.ProKeys where

import Data.Rhythm.RockBand.Common
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Data.Rhythm.RockBand.Lex.MIDI as MIDI
import Data.Rhythm.Event
import Data.Rhythm.Time
import Data.List (stripPrefix)
import qualified Numeric.NonNegative.Class as NNC

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

-- | For duration format, not switch format.
fromMIDI :: (NNC.C t) => MIDI.T t -> Maybe [T t]
fromMIDI (Long len (MIDI.Note _ p _)) = case V.fromPitch p of
  0 -> Just [Point $ LaneShift C]
  2 -> Just [Point $ LaneShift D]
  4 -> Just [Point $ LaneShift E]
  5 -> Just [Point $ LaneShift F]
  7 -> Just [Point $ LaneShift G]
  9 -> Just [Point $ LaneShift A]
  i | 48 <= i && i <= 72 -> Just [Long len (Note p)]
  115 -> Just [Long len Solo]
  116 -> Just [Long len Overdrive]
  120 -> Just [Long len BRE]
  126 -> Just [Long len Glissando]
  127 -> Just [Long len Trill]
  _ -> Nothing
fromMIDI (Point (MIDI.TextEvent str)) = case str of
  (readMood -> Just m) -> Just [Point $ Mood m]
  (stripPrefix "[begin_key song_trainer_key_" -> Just (reads -> [(n, "]")]))
    -> Just [Point $ Trainer $ TrainerBegin n]
  (stripPrefix "[end_key song_trainer_key_" -> Just (reads -> [(n, "]")]))
    -> Just [Point $ Trainer $ TrainerEnd n]
  _ -> Nothing
fromMIDI _ = Nothing

toMIDI :: T Beats -> MIDI.T Beats
toMIDI (Point p) = case p of
  LaneShift rng -> MIDI.blip $ V.toPitch $ [0, 2, 4, 5, 7, 9] !! fromEnum rng
  Trainer t -> Point $ MIDI.TextEvent $ case t of
    TrainerBegin n -> "[begin_key song_trainer_key_" ++ show n ++ "]"
    TrainerEnd   n -> "[end_key song_trainer_key_"   ++ show n ++ "]"
    TrainerNorm  n -> "[norm_key song_trainer_key_"  ++ show n ++ "]"
    -- norm doesn't actually show up in any HMX keys trainer sections
  Mood m -> Point $ MIDI.TextEvent $ showMood m
toMIDI (Long len l) = Long len $ MIDI.standardNote $ case l of
  Solo -> V.toPitch 115
  Glissando -> V.toPitch 126
  Trill -> V.toPitch 127
  Overdrive -> V.toPitch 116
  BRE -> V.toPitch 120
  Note p -> p
