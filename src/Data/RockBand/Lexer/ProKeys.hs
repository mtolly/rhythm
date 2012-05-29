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
  -- | A keyboard solo section.
  = Solo
  -- | Place over a sequence of white notes to make invisible fill lanes.
  | Glissando
  -- | Fill lanes between two keys.
  | Trill
  -- | Overdrive energy phrase.
  | Overdrive
  -- | The fill lanes for a Big Rock Ending.
  | BRE
  -- | Only pitches within the range of 48 and 72 are permitted.
  | Note V.Pitch
  deriving (Eq, Ord, Show)

{- | There are six playable ranges, each of which covers 10 white keys, plus
     all the black keys within. They are named here according to their lowest
     key. -}
data LaneRange = C | D | E | F | G | A
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

readEvent :: MIDI.Event Bool -> Maybe [Event Bool]
readEvent (Duration (MIDI.Note _ p _) b) = case V.fromPitch p of
  0 -> Just [Point $ LaneShift C | b]
  2 -> Just [Point $ LaneShift D | b]
  4 -> Just [Point $ LaneShift E | b]
  5 -> Just [Point $ LaneShift F | b]
  7 -> Just [Point $ LaneShift G | b]
  9 -> Just [Point $ LaneShift A | b]
  i | 48 <= i && i <= 72 -> Just [Duration (Note p) b]
  115 -> Just [Duration Solo b]
  116 -> Just [Duration Overdrive b]
  120 -> Just [Duration BRE b]
  126 -> Just [Duration Glissando b]
  127 -> Just [Duration Trill b]
  _ -> Nothing
readEvent (Point (MIDI.TextEvent str)) = case str of
  (readMood -> Just m) -> Just [Point $ Mood m]
  (stripPrefix "[begin_key song_trainer_key_" -> Just (reads -> [(n, "]")]))
    -> Just [Point $ Trainer $ TrainerBegin n]
  (stripPrefix "[end_key song_trainer_key_" -> Just (reads -> [(n, "]")]))
    -> Just [Point $ Trainer $ TrainerEnd n]
  _ -> Nothing
readEvent _ = Nothing
