{-# LANGUAGE ViewPatterns #-}
{- | The events found in the \"PART VOCAL\", \"HARM1\", \"HARM2\", and
     \"HARM3\" tracks. -}
module Data.RockBand.Lexer.Vocal where

import Data.RockBand.Common
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Data.RockBand.Lexer.MIDI as MIDI
import Data.MusicTime

type Event = TimeEvent Duration Point

data Point
  -- | An overdrive phrase is simultaneous with a 'Phrase', so the actual length
  -- of the overdrive note doesn't need to be recorded.
  = Overdrive
  | LyricShift
  | Mood Mood
  | Lyric String
  -- | A playable percussion note.
  | Percussion
  -- | A nonplayable percussion note, which just triggers the sound sample.
  | PercussionSound
  | PercussionAnimation PercussionType Bool
  deriving (Eq, Ord, Show, Read)

data Duration
  -- | General phrase marker (RB3) or Player 1 phrases (pre-RB3).
  = Phrase
  -- | Pre-RB3, used for 2nd player phrases in Tug of War.
  | Phrase2
  | RangeShift
  -- | Pitches from 36 to 84 are valid.
  | Note V.Pitch
  deriving (Eq, Ord, Show)

data PercussionType
  = Tambourine
  | Cowbell
  | Clap
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

readEvent :: MIDI.Event Bool -> Maybe [Event Bool]
readEvent (Duration (MIDI.Note _ p _) b) = case V.fromPitch p of
  0 -> Just [Duration RangeShift b]
  1 -> Just [Point LyricShift | b]
  i | 36 <= i && i <= 84 -> Just [Duration (Note p) b]
  96 -> Just [Point Percussion | b]
  97 -> Just [Point PercussionSound | b]
  105 -> Just [Duration Phrase b]
  106 -> Just [Duration Phrase2 b]
  116 -> Just [Point Overdrive | b]
  _ -> Nothing
readEvent (Point (MIDI.Lyric str)) = Just [Point $ Lyric str]
readEvent (Point (MIDI.TextEvent str)) = case str of
  (readPercAnim -> Just evt) -> Just [Point evt]
  (readMood     -> Just md ) -> Just [Point $ Mood md]
  -- HMX often (accidentally?) uses text events for lyrics.
  -- They appear to work in-game nonetheless.
  _ -> Just [Point $ Lyric str]

readPercAnim :: String -> Maybe Point
readPercAnim str = case str of
  "[tambourine_start]" -> f Tambourine True
  "[tambourine_end]" -> f Tambourine False
  "[cowbell_start]" -> f Cowbell True
  "[cowbell_end]" -> f Cowbell False
  "[clap_start]" -> f Clap True
  "[clap_end]" -> f Clap False
  _ -> Nothing
  where f typ b = Just $ PercussionAnimation typ b
