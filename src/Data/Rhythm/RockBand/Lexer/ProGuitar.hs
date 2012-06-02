{-# LANGUAGE ViewPatterns #-}
-- | The contents of the \"PART REAL_GUITAR\", \"PART REAL_GUITAR_22\",
-- \"PART REAL_BASS\", and \"PART REAL_BASS_22\" tracks.
module Data.Rhythm.RockBand.Lexer.ProGuitar where

import Data.Rhythm.RockBand.Common
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Data.Rhythm.RockBand.Lexer.MIDI as MIDI
import Data.Rhythm.Types
import Data.List (stripPrefix)

data GtrString = S6 | S5 | S4 | S3 | S2 | S1
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

type Event = TimeEvent Duration Point

data Point
  = TrainerGtr Trainer
  | TrainerBass Trainer
  | HandPosition GtrFret
  -- | Valid pitches are 4 (E) to 15 (D#).
  | ChordRoot V.Pitch
  -- | A shortcut encoding for a note without sustain. A 'Note' with duration
  -- of a sixteenth note (1/4 a quarter note) or less is equivalent.
  | PointNote GtrString GtrFret NoteType
  deriving (Eq, Ord, Show)

data Duration
  = Trill
  | Tremolo
  | BRE
  | Overdrive
  | Solo
  | NoChordNames
  | SlashChords
  | DiffEvent Difficulty DiffEvent
  deriving (Eq, Ord, Show, Read)

data DiffEvent
  = Note GtrString GtrFret NoteType
  | AllFrets
  | LowChord
  | MidChord
  | HighChord
  | Arpeggio
  -- Does Slide need to have the "a" parameter?
  | Slide SlideType
  | ForceHOPO
  deriving (Eq, Ord, Show, Read)

data NoteType
  = NormalNote
  | ArpeggioForm
  | Bent
  | Muted
  | Tapped
  | Harmonic
  | PinchHarmonic
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data SlideType
  = NormalSlide
  | ReversedSlide
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Designed only for duration format, not switch format.
readEvent :: MIDI.Event dur -> Maybe [Event dur]
readEvent (Duration len (MIDI.Note ch p vel)) = case V.fromPitch p of
  -- TODO
  126 -> Just [Duration len Tremolo]
  127 -> Just [Duration len Trill]
  _ -> Nothing
readEvent (Point (MIDI.TextEvent str)) = case str of
  (stripPrefix "[begin_pg song_trainer_pg_" -> Just (reads -> [(n, "]")]))
    -> Just [Point $ TrainerGtr $ TrainerBegin n]
  (stripPrefix "[pg_norm song_trainer_pg_" -> Just (reads -> [(n, "]")]))
    -> Just [Point $ TrainerGtr $ TrainerNorm n]
  (stripPrefix "[end_pg song_trainer_pg_" -> Just (reads -> [(n, "]")]))
    -> Just [Point $ TrainerGtr $ TrainerEnd n]
  _ -> Nothing
readEvent _ = Nothing
