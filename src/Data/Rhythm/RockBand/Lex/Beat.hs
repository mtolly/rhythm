-- | The contents of the \"BEAT\" track.
module Data.Rhythm.RockBand.Lex.Beat where

import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Data.Rhythm.MIDI as MIDI
import Data.Rhythm.Event
import Data.Rhythm.Time
import Data.Rhythm.RockBand.Common
import qualified Numeric.NonNegative.Class as NN
import Data.Rhythm.Parser

data T
  = Bar -- ^ A thick barline; the beginning of a new measure.
  | Beat -- ^ A thin barline; a beat in the middle of a measure.
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

parse :: (NN.C a) => Parser (MIDI.T a) T
parse = get >>= \x -> case x of
  Length _ n@(MIDI.Note _ p _) -> case V.fromPitch p of
    12 -> return Bar
    13 -> return Beat
    _ -> unrecognized n
  Point p -> unrecognized p

unparse :: T -> MIDI.T Beats
unparse b = blip $ V.toPitch $ case b of Bar -> 12; Beat -> 13
