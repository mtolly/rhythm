{-# LANGUAGE ViewPatterns, PatternGuards, MultiParamTypeClasses #-}
-- | The contents of the \"PART GUITAR\/BASS\/KEYS\" tracks.
module Data.Rhythm.RockBand.Lex.Basic where

import Data.Rhythm.RockBand.Common
import qualified Sound.MIDI.Message.Channel.Voice as V
import Control.Monad
import Data.Rhythm.Event
import Data.List (stripPrefix)
import qualified Data.Rhythm.RockBand.Lex.MIDI as MIDI
import Control.Applicative

data Long
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
  deriving (Eq, Ord, Show, Read)

instance Duration Long Point where
  condense x@(AtFret _) (AtFret _) = Just x
  condense x y = guard (x == y) >> Just x
type T = Event Long Point

data DiffEvent
  = Note Fret
  | ForceHopo
  | ForceStrum
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
fromMIDI :: MIDI.T t -> Maybe [T t]
fromMIDI (Long b (MIDI.Note _ p _)) = case V.fromPitch p of
  i | 40 <= i && i <= 59 -> Just [Long b (AtFret (GtrFret $ i - 40))]
  i | let (oct, k) = quotRem i 12
    , 5 <= oct && oct <= 8
    , 0 <= k && k <= 6
    , let diff = toEnum $ oct - 5
          evt = case k of
            5 -> ForceHopo
            6 -> ForceStrum
            _ -> Note (toEnum k)
    -> Just [Long b (DiffEvent diff evt)]
  103 -> Just [Long b Solo]
  105 -> Just [Long b Player1]
  106 -> Just [Long b Player2]
  116 -> Just [Long b Overdrive]
  120 -> Just [Long b BRE]
  121 -> Just []
  122 -> Just []
  123 -> Just []
  124 -> Just []
  126 -> Just [Long b Tremolo]
  127 -> Just [Long b Trill]
  _ -> Nothing
fromMIDI (Point (MIDI.TextEvent str)) = case str of
  (readMood -> Just m) -> Just [Point $ Mood m]
  (readHandMap -> Just hm) -> Just [Point $ HandMap hm]
  (readStrumMap -> Just sm) -> Just [Point $ StrumMap sm]
  _ -> Nothing
fromMIDI _ = Nothing

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

showHandMap :: HandMap -> String
showHandMap HandDefault = "[map HandMap_Default]"
showHandMap NoChords = "[map HandMap_NoChords]"
showHandMap AllChords = "[map HandMap_AllChords]"
showHandMap HandSolo = "[map HandMap_Solo]"
showHandMap DropD = "[map HandMap_DropD]"
showHandMap DropD2 = "[map HandMap_DropD2]"
showHandMap AllBend = "[map HandMap_AllBend]"
showHandMap ChordC = "[map HandMap_Chord_C]"
showHandMap ChordD = "[map HandMap_Chord_D]"
showHandMap ChordA = "[map HandMap_Chord_A]"

readStrumMap :: String -> Maybe StrumMap
readStrumMap = stripPrefix "[map StrumMap_" >=> \str -> case str of
  "Default]" -> Just StrumDefault
  "Pick]" -> Just Pick
  "SlapBass]" -> Just SlapBass
  _ -> Nothing

showStrumMap :: StrumMap -> String
showStrumMap StrumDefault = "[map StrumMap_Default]"
showStrumMap Pick = "[map StrumMap_Pick]"
showStrumMap SlapBass = "[map StrumMap_SlapBass]"

toMIDI :: T t -> [MIDI.T t]
toMIDI (Point p) = (:[]) . Point . MIDI.TextEvent $ case p of
  Mood m -> showMood m
  HandMap hm -> showHandMap hm
  StrumMap sm -> showStrumMap sm
toMIDI (Long len l) = Long len . MIDI.standardNote . V.toPitch <$> case l of
  Solo -> [103]
  Tremolo -> [126]
  Trill -> [127]
  Overdrive -> [116]
  BRE -> [120..124]
  Player1 -> [105]
  Player2 -> [106]
  AtFret f -> [fromGtrFret f + 40]
  DiffEvent diff evt -> case evt of
    Note f -> [base + fromEnum f]
    ForceHopo -> [base + 5]
    ForceStrum -> [base + 6]
    where base = 60 + 12 * fromEnum diff
