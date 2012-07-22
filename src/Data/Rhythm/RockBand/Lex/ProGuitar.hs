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
import Data.Char (isSpace)
import Data.List (stripPrefix)

data GtrString = S6 | S5 | S4 | S3 | S2 | S1
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Duration Long Point
type T = Event Long Point

data Point
  = TrainerGtr Trainer
  | TrainerBass Trainer
  | HandPosition GtrFret
  | ChordRoot V.Pitch -- ^ Valid pitches are 4 (E) to 15 (D#).
  | ChordName Difficulty String
  | UnknownPitch18 C.Channel V.Velocity
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
  deriving (Eq, Ord, Show)

data DiffEvent
  = Note GtrString GtrFret NoteType
  | ForceHOPO
  | Slide SlideType
  | Arpeggio
  | PartialChord StrumArea
  | UnknownBFlat C.Channel V.Velocity
  | AllFrets
  -- ChordName String
  deriving (Eq, Ord, Show)

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
    i | 4 <= i && i <= 15 -> single $ Point $ ChordRoot p
    16 -> single $ Long len SlashChords
    17 -> single $ Long len NoChordNames
    18 -> single $ Point $ UnknownPitch18 ch vel
    i | let (oct, k) = quotRem i 12
      , elem oct [2,4,6,8]
      , let longDiff = Long len . DiffEvent (toEnum $ quot oct 2 - 1)
      -> case k of
        6 -> single $ longDiff ForceHOPO
        7 -> case C.fromChannel ch of
          0   -> single $ longDiff $ Slide NormalSlide
          11  -> single $ longDiff $ Slide ReversedSlide
          ch' -> warn w >> single (longDiff $ Slide NormalSlide) where
            w = "Slide marker (pitch " ++ show i ++ ") has unknown channel " ++ show ch'
        8 -> single $ longDiff Arpeggio
        9 -> case C.fromChannel ch of
          13 -> single $ longDiff $ PartialChord High
          14 -> single $ longDiff $ PartialChord Mid
          15 -> single $ longDiff $ PartialChord Low
          ch'  -> warn w >> single (longDiff $ PartialChord Mid) where
            w = "Partial chord marker (pitch " ++ show i ++ ") has unknown channel " ++ show ch'
        10 -> single $ longDiff $ UnknownBFlat ch vel
        11 -> single $ longDiff AllFrets
        _ -> single $ longDiff $ Note nstr nfret ntyp where
          nstr = toEnum k
          nfret = GtrFret $ V.fromVelocity vel - 100
          ntyp = toEnum $ C.fromChannel ch
    108 -> single $ Point $ HandPosition $ GtrFret $ V.fromVelocity vel - 100
    115 -> single $ Long len Solo
    116 -> single $ Long len Overdrive
    120 -> single $ Long len BRE
    121 -> return []
    122 -> return []
    123 -> return []
    124 -> return []
    125 -> return []
    126 -> single $ Long len Tremolo
    127 -> single $ Long len Trill
    _ -> none
  interpret (Point (MIDI.TextEvent str)) = case readTrainer str of
    Just (t, "pg") -> single $ Point $ TrainerGtr t
    Just (t, "pb") -> single $ Point $ TrainerBass t
    _ -> case readChordName str of
      Nothing -> none
      Just (diff, name) -> single $ Point $ ChordName diff name
  interpret _ = none

readChordName :: String -> Maybe (Difficulty, String)
readChordName str
  | Just (x:xs) <- stripPrefix "[chrd" str
  , elem x "0123"
  , let diff = toEnum $ read [x]
  , Just name <- MIDI.stripSuffix "]" $ dropWhile isSpace xs
  = Just (diff, name)
  | otherwise = Nothing
