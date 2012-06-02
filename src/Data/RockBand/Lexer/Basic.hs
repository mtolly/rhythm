{-# LANGUAGE ViewPatterns, PatternGuards #-}
-- | The contents of the \"PART GUITAR\/BASS\/KEYS\" tracks.
module Data.RockBand.Lexer.Basic where

import Data.RockBand.Common
import qualified Sound.MIDI.Message.Channel.Voice as V
import Control.Monad
import Data.MusicTime
import Data.List (stripPrefix)
import qualified Data.RockBand.Lexer.MIDI as MIDI

type Event = TimeEvent Duration Point

data Duration
  = Solo
  | Tremolo
  | Trill
  | Overdrive
  | BRE
  | Player1 -- ^ Used pre-RB3 for Tug of War mode.
  | Player2 -- ^ Used pre-RB3 for Tug of War mode.
  | AtFret GtrFret -- ^ Frets 0..19 (pitches 40..59 in the MIDI) are valid.
  | DiffEvent Difficulty DiffEvent
  deriving (Eq, Ord, Show, Read)

data Point
  = Mood Mood
  | HandMap HandMap
  | StrumMap StrumMap
  -- | A shortcut encoding for a note without sustain. A 'Note' with duration
  -- of a sixteenth note (1/4 a quarter note) or less is equivalent.
  | PointNote Fret
  deriving (Eq, Ord, Show, Read)

data DiffEvent
  = ForceHopo
  | ForceStrum
  | Note Fret
  deriving (Eq, Ord, Show, Read)

data Fret
  = Green
  | Red
  | Yellow
  | Blue
  | Orange
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Controls the fretting hand animation of a guitarist/bassist.
data HandMap
  -- | Normal fingering. Single gems = single fingers, gems with duration =
  -- vibrato, chord gems = chords.
  = HandDefault
  | NoChords -- ^ All single fingers/vibrato.
  | AllChords -- ^ All chords.
  | HandSolo -- ^ D major shape for all chords, vibrato for all chord sustains.
  | DropD -- ^ Open hand for all green gems, all other gems are chords.
  | DropD2 -- ^ Open hand for all green gems.
  | AllBend -- ^ All ring finger high vibrato.
  | ChordC -- ^ All C chord shape.
  | ChordD -- ^ All D chord shape.
  | ChordA -- ^ All A minor chord shape.
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Controls the strumming animation for a bassist.
data StrumMap
  = StrumDefault -- ^ Fingered
  | Pick
  | SlapBass
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Works with both switch and duration format; a 'Duration' always maps to a
-- 'Duration' and a 'Point' always maps to a 'Point'.
readEvent :: MIDI.Event a -> Maybe [Event a]
readEvent (Duration b (MIDI.Note _ p _)) = case V.fromPitch p of
  i | 40 <= i && i <= 59 -> Just [Duration b (AtFret (GtrFret $ i - 40))]
  i | let (oct, k) = quotRem i 12
    , 5 <= oct && oct <= 8
    , 0 <= k && k <= 6
    , let diff = toEnum $ oct - 5
          evt = case k of
            5 -> ForceHopo
            6 -> ForceStrum
            _ -> Note (toEnum k)
    -> Just [Duration b (DiffEvent diff evt)]
  103 -> Just [Duration b Solo]
  105 -> Just [Duration b Player1]
  106 -> Just [Duration b Player2]
  116 -> Just [Duration b Overdrive]
  120 -> Just [Duration b BRE]
  121 -> Just []
  122 -> Just []
  123 -> Just []
  124 -> Just []
  126 -> Just [Duration b Tremolo]
  127 -> Just [Duration b Trill]
  _ -> Nothing
readEvent (Point (MIDI.TextEvent str)) = case str of
  (readMood -> Just m) -> Just [Point $ Mood m]
  (readHandMap -> Just hm) -> Just [Point $ HandMap hm]
  (readStrumMap -> Just sm) -> Just [Point $ StrumMap sm]
  _ -> Nothing
readEvent _ = Nothing

readHandMap :: String -> Maybe HandMap
readHandMap = stripPrefix "[map HandMap_" >=> \str -> case str of
  "Default]" -> Just HandDefault
  "NoChords]" -> Just NoChords
  "AllChords]" -> Just AllChords
  "Solo]" -> Just HandSolo
  "DropD]" -> Just DropD
  "DropD2]" -> Just DropD2
  "AllBend]" -> Just AllBend
  "Chord_C]" -> Just ChordC
  "Chord_D]" -> Just ChordD
  "Chord_A]" -> Just ChordA
  _ -> Nothing

readStrumMap :: String -> Maybe StrumMap
readStrumMap = stripPrefix "[map StrumMap_" >=> \str -> case str of
  "Default]" -> Just StrumDefault
  "Pick]" -> Just Pick
  "SlapBass]" -> Just SlapBass
  _ -> Nothing
