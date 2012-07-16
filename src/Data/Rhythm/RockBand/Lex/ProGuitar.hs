{-# LANGUAGE MultiParamTypeClasses, PatternGuards, FlexibleInstances, TypeSynonymInstances #-}
-- | The contents of the \"PART REAL_GUITAR\", \"PART REAL_GUITAR_22\",
-- \"PART REAL_BASS\", and \"PART REAL_BASS_22\" tracks.
module Data.Rhythm.RockBand.Lex.ProGuitar where

import Data.Rhythm.RockBand.Common
import qualified Sound.MIDI.Message.Channel as C
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Data.Rhythm.RockBand.Lex.MIDI as MIDI
import Data.Rhythm.Event
import Data.Rhythm.Interpret
import qualified Numeric.NonNegative.Class as NNC

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
  -- ChordName String
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

instance (NNC.C a) => Interpret (MIDI.T a) (T a) where
  interpret (Long len (MIDI.Note ch p vel)) = case V.fromPitch p of
    i | 4 <= i && i <= 15 -> ok $ Point $ ChordRoot p
    16 -> ok $ Long len SlashChords
    17 -> ok $ Long len NoChordNames
    i | let (oct, k) = quotRem i 12
      , elem oct [2,4,6,8]
      , let longDiff = Long len . DiffEvent (toEnum $ quot oct 2 - 1)
      -> case k of
        6 -> ok $ longDiff ForceHOPO
        7 -> case C.fromChannel ch of
          1   -> ok $ longDiff $ Slide NormalSlide
          11  -> ok $ longDiff $ Slide ReversedSlide
          ch' -> Just ([longDiff $ Slide NormalSlide],
            ["Slide marker (pitch " ++ show i ++ ") has unknown channel " ++ show ch'])
        8 -> ok $ longDiff Arpeggio
        9 -> case C.fromChannel ch of
          13 -> ok $ longDiff $ PartialChord High
          14 -> ok $ longDiff $ PartialChord Mid
          15 -> ok $ longDiff $ PartialChord Low
          ch'  -> Just ([longDiff $ PartialChord Mid],
            ["Partial chord marker (pitch " ++ show i ++ ") has unknown channel " ++ show ch'])
        10 -> ok $ longDiff $ UnknownBFlat
        11 -> ok $ longDiff AllFrets
        _ -> ok $ longDiff $ Note nstr nfret ntyp where
          nstr = toEnum k
          nfret = GtrFret $ V.fromVelocity vel - 100
          ntyp = toEnum $ C.fromChannel ch
    108 -> ok $ Point $ HandPosition $ GtrFret $ V.fromVelocity vel - 100
    115 -> ok $ Long len Solo
    116 -> ok $ Long len Overdrive
    120 -> ok $ Long len BRE
    121 -> okList []
    122 -> okList []
    123 -> okList []
    124 -> okList []
    125 -> okList []
    126 -> ok $ Long len Tremolo
    127 -> ok $ Long len Trill
    _ -> Nothing
  interpret (Point (MIDI.TextEvent str)) = case readTrainer str of
    Just (t, "pg") -> ok $ Point $ TrainerGtr t
    Just (t, "pb") -> ok $ Point $ TrainerBass t
    _ -> Nothing
  interpret _ = Nothing
