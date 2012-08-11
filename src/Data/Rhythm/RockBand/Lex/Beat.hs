-- | The contents of the \"BEAT\" track.
module Data.Rhythm.RockBand.Lex.Beat where

import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Data.Rhythm.RockBand.Lex.MIDI as MIDI
import Data.Rhythm.Event
import Data.Rhythm.Time
import qualified Numeric.NonNegative.Class as NN
import Data.Rhythm.Interpret

data T
  = Bar -- ^ A thick barline; the beginning of a new measure.
  | Beat -- ^ A thin barline; a beat in the middle of a measure.
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

interpret :: (NN.C t) => Interpreter (MIDI.T t) T
interpret (Length _ (MIDI.Note _ p _)) = case V.fromPitch p of
  12 -> single Bar
  13 -> single Beat
  _ -> none
interpret _ = none

uninterpret :: Uninterpreter T (MIDI.T Beats)
uninterpret b = (:[]) $ MIDI.blip $ V.toPitch $ case b of Bar -> 12; Beat -> 13
