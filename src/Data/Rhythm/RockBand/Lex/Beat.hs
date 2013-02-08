-- | The contents of the \"BEAT\" track.
module Data.Rhythm.RockBand.Lex.Beat where

import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Data.Rhythm.MIDI as MIDI
import Data.Rhythm.Event
import Data.Rhythm.Time
import Data.Rhythm.RockBand.Common
import qualified Numeric.NonNegative.Class as NN
import Data.Rhythm.Parser
import qualified Data.EventList.Relative.TimeBody as RTB
import Data.Rhythm.TimeSignature

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

-- | Given a list of signatures for each measure, produces a default BEAT track.
-- Each time signature's unit must be at least 1/2 beat, or exactly 1/4 beat.
-- (That is, the denominator must be no more than 8, or exactly 16.)
beatTrack :: [TimeSignature] -> RTB.T Beats T
beatTrack [] = RTB.empty
beatTrack (TimeSignature 0 _ : _) = error "beatTrack: 0 multiplier in signature"
beatTrack (TimeSignature mult unit : sigs) = if unit >= 0.5
  then let -- Bar Beat Beat
    addBar = RTB.cons 0 Bar . RTB.delay unit
    addBeat = RTB.cons 0 Beat . RTB.delay unit
    in addBar $ foldr (.) id (replicate (fromIntegral mult - 1) addBeat) $
      beatTrack sigs
  else case unit of
    0.25 -> let
      addBar = RTB.cons 0 Bar . RTB.delay (unit * 2)
      addBeat = RTB.cons 0 Beat . RTB.delay (unit * 2)
      evenBeatNum = fromIntegral $ (mult `quot` 2) - 1
      evenBeats = addBar . foldr (.) id (replicate evenBeatNum addBeat)
      in if even mult
        then evenBeats $ beatTrack sigs -- Bar | Beat | Beat |
        else evenBeats $ RTB.delay unit $ beatTrack sigs -- Bar | Bt | Bt | |
    _ -> error $ "beatTrack: unsupported time signature unit " ++ show unit
