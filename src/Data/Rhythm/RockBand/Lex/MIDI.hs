-- | A simpler format to hold only the MIDI events used in Rock Band files.
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Rhythm.RockBand.Lex.MIDI where

import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as M
import qualified Sound.MIDI.Message.Channel as C
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Numeric.NonNegative.Class as NNC
import qualified Data.EventList.Relative.TimeBody as RTB
import Data.Rhythm.Time
import Data.Rhythm.Event
import Control.Monad.Trans.State
import Data.List (stripPrefix)
import Control.Monad (guard)

data Long
  = Note C.Channel V.Pitch V.Velocity
  deriving (Eq, Ord, Show)

data Point
  = TextEvent String
  | Lyric String
  deriving (Eq, Ord, Show, Read)

instance Duration Long Point where
  -- | A note-on and note-off are connected if their pitches are equal.
  condense x@(Note _ px _) (Note _ py _) = guard (px == py) >> Just x
type T = Event Long Point

fromMIDITrack :: (NNC.C t) => RTB.T t E.T -> RTB.T t (T Bool)
fromMIDITrack = RTB.mapMaybe fromMIDI

fromMIDI :: E.T -> Maybe (T Bool)
fromMIDI (E.MIDIEvent (C.Cons ch (C.Voice v))) = case V.explicitNoteOff v of
  V.NoteOff p vel -> Just $ Long False $ Note ch p vel
  V.NoteOn p vel -> Just $ Long True $ Note ch p vel
  _ -> Nothing
fromMIDI (E.MetaEvent (M.TextEvent str)) = Just $ Point $ TextEvent str
fromMIDI (E.MetaEvent (M.Lyric str)) = Just $ Point $ Lyric str
fromMIDI _ = Nothing

toMIDI :: T Bool -> E.T
toMIDI (Long b (Note c p v)) =
  E.MIDIEvent $ C.Cons c $ C.Voice $ (if b then V.NoteOff else V.NoteOn) p v
toMIDI (Point p) = E.MetaEvent $ case p of
  TextEvent str -> M.TextEvent str
  Lyric str -> M.Lyric str

standardNote :: V.Pitch -> Long
standardNote p = Note (C.toChannel 0) p (V.toVelocity 96)

-- | Creates a MIDI note of the minimum possible duration allowed by Rock Band.
-- In a valid Rock Band MIDI, this is guaranteed to not overlap other notes.
blip :: V.Pitch -> T Beats
blip p = Long (1 / 32) $ standardNote p

-- | Extracts and converts events of type 'b', while leaving behind unrecognized
-- events of type 'a'.
extract :: (NNC.C t) => (a -> Maybe [b]) -> State (RTB.T t a) (RTB.T t b)
extract f = fmap RTB.flatten $ state $ RTB.partitionMaybe f

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix xs ys = fmap reverse $ stripPrefix (reverse xs) (reverse ys)
