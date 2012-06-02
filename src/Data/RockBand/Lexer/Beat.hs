-- | The contents of the \"BEAT\" track.
module Data.RockBand.Lexer.Beat where

import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Data.RockBand.Lexer.MIDI as MIDI
import Data.MusicTime

data Event
  -- | A thick barline; the beginning of a new measure.
  = Bar
  -- | A thin barline; a beat in the middle of a measure.
  | Beat
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Designed only for duration format, not switch format.
readEvent :: MIDI.Event dur -> Maybe [Event]
readEvent (Duration _ (MIDI.Note _ p _)) = case V.fromPitch p of
  12 -> Just [Bar]
  13 -> Just [Beat]
  _  -> Nothing
readEvent _ = Nothing
