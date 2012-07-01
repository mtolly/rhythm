{-# LANGUAGE ViewPatterns, MultiParamTypeClasses #-}
{- | The events found in the \"PART VOCALS\", \"HARM1\", \"HARM2\", and
     \"HARM3\" tracks. -}
module Data.Rhythm.RockBand.Lex.Vocals where

import Data.Rhythm.RockBand.Common
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Data.Rhythm.RockBand.Lex.MIDI as MIDI
import Data.Rhythm.Event

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

data Long
  -- | General phrase marker (RB3) or Player 1 phrases (pre-RB3).
  = Phrase
  -- | Pre-RB3, used for 2nd player phrases in Tug of War.
  | Phrase2
  | RangeShift
  -- | Pitches from 36 to 84 are valid.
  | Note V.Pitch
  deriving (Eq, Ord, Show)

instance Duration Long Point
type T = Event Long Point

data PercussionType
  = Tambourine
  | Cowbell
  | Clap
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Designed only for duration format, not switch format.
readEvent :: MIDI.T dur -> Maybe [T dur]
readEvent (Long len (MIDI.Note _ p _)) = case V.fromPitch p of
  0 -> Just [Long len RangeShift]
  1 -> Just [Point LyricShift]
  i | 36 <= i && i <= 84 -> Just [Long len $ Note p]
  96 -> Just [Point Percussion]
  97 -> Just [Point PercussionSound]
  105 -> Just [Long len Phrase]
  106 -> Just [Long len Phrase2]
  116 -> Just [Point Overdrive]
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
