-- | The contents of the \"BEAT\" track.
module Data.Rhythm.RockBand.Lex.Beat where

import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Data.Rhythm.RockBand.Lex.MIDI as MIDI
import Data.Rhythm.Event
import Data.Rhythm.Time

data T
  = Bar -- ^ A thick barline; the beginning of a new measure.
  | Beat -- ^ A thin barline; a beat in the middle of a measure.
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

fromMIDI :: MIDI.T dur -> Maybe T
fromMIDI (Long _ (MIDI.Note _ p _)) = case V.fromPitch p of
  12 -> Just Bar
  13 -> Just Beat
  _  -> Nothing
fromMIDI _ = Nothing

toMIDI :: T -> MIDI.T Beats
toMIDI Bar = MIDI.blip $ V.toPitch 12
toMIDI Beat = MIDI.blip $ V.toPitch 13
