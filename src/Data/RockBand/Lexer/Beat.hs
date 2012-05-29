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

readEvent :: MIDI.Event Bool -> Maybe [Event]
readEvent (Duration (MIDI.Note _ p _) b) = case V.fromPitch p of
  12 -> Just [Bar | b]
  13 -> Just [Beat | b]
  _  -> Nothing
readEvent _ = Nothing
