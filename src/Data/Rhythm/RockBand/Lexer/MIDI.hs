-- | A simpler format to hold only the MIDI events used in Rock Band files.
module Data.Rhythm.RockBand.Lexer.MIDI where

import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as M
import qualified Sound.MIDI.Message.Channel as C
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Numeric.NonNegative.Class as NNC
import qualified Data.EventList.Relative.TimeBody as RTB
import Data.Rhythm.Types
import Control.Monad.Trans.State
import Data.List (stripPrefix)

type Event = TimeEvent Duration Point

data Duration
  = Note C.Channel V.Pitch V.Velocity
  -- "Duration True (Note _ _ 0)" should never happen
  deriving (Eq, Ord, Show)

data Point
  = TextEvent String
  | Lyric String
  deriving (Eq, Ord, Show, Read)

-- | A note-on and note-off are connected if their pitches are equal.
pitchEqual :: Duration -> Duration -> Bool
pitchEqual (Note _ p1 _) (Note _ p2 _) = p1 == p2

fromMIDITrack :: (NNC.C t) => RTB.T t E.T -> RTB.T t (Event Bool)
fromMIDITrack = RTB.mapMaybe fromMIDI

fromMIDI :: E.T -> Maybe (Event Bool)
fromMIDI (E.MIDIEvent (C.Cons ch (C.Voice v))) = case V.explicitNoteOff v of
  V.NoteOff p vel -> Just $ Duration False $ Note ch p vel
  V.NoteOn p vel -> Just $ Duration True $ Note ch p vel
  _ -> Nothing
fromMIDI (E.MetaEvent (M.TextEvent str)) = Just $ Point $ TextEvent str
fromMIDI (E.MetaEvent (M.Lyric str)) = Just $ Point $ Lyric str
fromMIDI _ = Nothing

standardNote :: V.Pitch -> Duration
standardNote p = Note (C.toChannel 0) p (V.toVelocity 96)

-- | Creates a MIDI note of the minimum possible duration allowed by Rock Band.
-- In a valid Rock Band MIDI, this is guaranteed to not overlap other notes.
blip :: V.Pitch -> Event Beats
blip p = Duration (1 / 32) $ standardNote p

-- | Extracts and converts events of type 'b', while leaving behind unrecognized
-- events of type 'a'.
extract :: (NNC.C t) => (a -> Maybe [b]) -> State (RTB.T t a) (RTB.T t b)
extract f = fmap RTB.flatten $ state $ RTB.partitionMaybe f

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix xs ys = fmap reverse $ stripPrefix (reverse xs) (reverse ys)
