{-# LANGUAGE ViewPatterns #-}
-- | The contents of the \"PART REAL_KEYS_?\" and \"KEYS_ANIM_?H\" tracks.
module Data.RockBand.Lexer.ProKeys where

import Data.RockBand.Common
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Data.RockBand.Lexer.MIDI as MIDI
import Data.MusicTime
import Data.List (stripPrefix)

type Event = TimeEvent Duration Point

data Point
  {- | Change the viewable play range. Should be placed at least a measure
       before any notes that require the new range. -}
  = LaneShift LaneRange
  -- | The beginning/end of Pro Keys trainer sections.
  | Trainer Trainer
  -- | A shortcut encoding for a note without sustain. A 'Note' with duration
  -- of a sixteenth note (1/4 a quarter note) or less is equivalent.
  | PointNote V.Pitch
  | Mood Mood
  deriving (Eq, Ord, Show)

data Duration
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

-- | Designed only for duration format, not switch format.
readEvent :: MIDI.Event dur -> Maybe [Event dur]
readEvent (Duration len (MIDI.Note _ p _)) = case V.fromPitch p of
  0 -> Just [Point $ LaneShift C]
  2 -> Just [Point $ LaneShift D]
  4 -> Just [Point $ LaneShift E]
  5 -> Just [Point $ LaneShift F]
  7 -> Just [Point $ LaneShift G]
  9 -> Just [Point $ LaneShift A]
  i | 48 <= i && i <= 72 -> Just [Duration len (Note p)]
  115 -> Just [Duration len Solo]
  116 -> Just [Duration len Overdrive]
  120 -> Just [Duration len BRE]
  126 -> Just [Duration len Glissando]
  127 -> Just [Duration len Trill]
  _ -> Nothing
readEvent (Point (MIDI.TextEvent str)) = case str of
  (readMood -> Just m) -> Just [Point $ Mood m]
  (stripPrefix "[begin_key song_trainer_key_" -> Just (reads -> [(n, "]")]))
    -> Just [Point $ Trainer $ TrainerBegin n]
  (stripPrefix "[end_key song_trainer_key_" -> Just (reads -> [(n, "]")]))
    -> Just [Point $ Trainer $ TrainerEnd n]
  _ -> Nothing
readEvent _ = Nothing
