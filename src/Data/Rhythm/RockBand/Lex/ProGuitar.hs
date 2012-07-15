{-# LANGUAGE MultiParamTypeClasses, PatternGuards #-}
-- | The contents of the \"PART REAL_GUITAR\", \"PART REAL_GUITAR_22\",
-- \"PART REAL_BASS\", and \"PART REAL_BASS_22\" tracks.
module Data.Rhythm.RockBand.Lex.ProGuitar where

import Data.Rhythm.RockBand.Common
import qualified Sound.MIDI.Message.Channel as C
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Data.Rhythm.RockBand.Lex.MIDI as MIDI
import Data.Rhythm.Event

data GtrString = S6 | S5 | S4 | S3 | S2 | S1
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Duration Long Point
type T = Event Long Point

data Point
  = TrainerGtr Trainer
  | TrainerBass Trainer
  | HandPosition GtrFret
  | ChordRoot V.Pitch -- ^ Valid pitches are 4 (E) to 15 (D#).
  deriving (Eq, Ord, Show)

data Long
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
  | ForceHOPO
  | Slide SlideType
  | Arpeggio
  | PartialChord StrumArea
  | UnknownBFlat
  | AllFrets
  -- | ChordName String
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

data SlideType = NormalSlide | ReversedSlide
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data StrumArea = Low | Mid | High
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Designed only for duration format, not switch format.
fromMIDI :: MIDI.T dur -> Maybe [T dur]
fromMIDI (Long len (MIDI.Note ch p vel)) = case V.fromPitch p of
  i | 4 <= i && i <= 15 -> Just [Point $ ChordRoot p]
  16 -> Just [Long len SlashChords]
  17 -> Just [Long len NoChordNames]
  i | let (oct, k) = quotRem i 12
    , elem oct [2,4,6,8]
    , let diff = toEnum $ quot oct 2 - 1
    -> Just $ (:[]) $ Long len $ DiffEvent diff $ case k of
      6 -> ForceHOPO
      7 -> Slide $ case C.fromChannel ch of
        11 -> ReversedSlide
        _  -> NormalSlide
      8 -> Arpeggio
      9 -> PartialChord $ case C.fromChannel ch of
        13 -> High
        14 -> Mid
        15 -> Low
        x  -> error $ "ProGuitar.fromMIDI: Unknown channel "
          ++ show x ++ " for partial chord marker"
      10 -> UnknownBFlat
      11 -> AllFrets
      _ -> Note (toEnum k) (GtrFret $ V.fromVelocity vel - 100)
        (toEnum $ C.fromChannel ch)
  108 -> Just [Point $ HandPosition $ GtrFret $ V.fromVelocity vel - 100]
  115 -> Just [Long len Solo]
  116 -> Just [Long len Overdrive]
  120 -> Just [Long len BRE]
  121 -> Just []
  122 -> Just []
  123 -> Just []
  124 -> Just []
  125 -> Just []
  126 -> Just [Long len Tremolo]
  127 -> Just [Long len Trill]
  _ -> Nothing
fromMIDI (Point (MIDI.TextEvent str)) = case readTrainer str of
  Just (t, "pg") -> Just [Point $ TrainerGtr t]
  Just (t, "pb") -> Just [Point $ TrainerBass t]
  _ -> Nothing
fromMIDI _ = Nothing
