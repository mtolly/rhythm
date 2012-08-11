-- | A simpler format to hold only the MIDI events used in Rock Band files.
module Data.Rhythm.RockBand.Lex.MIDI where

import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as M
import qualified Sound.MIDI.Message.Channel as C
import qualified Sound.MIDI.Message.Channel.Voice as V
import Data.Rhythm.Time
import Data.Rhythm.Event
import Data.List (stripPrefix)
import Data.Rhythm.Interpret

data Length
  = Note C.Channel V.Pitch V.Velocity
  deriving (Eq, Ord, Show)

data Point
  = TextEvent String
  | Lyric String
  deriving (Eq, Ord, Show, Read)

instance Long Length where
  -- | A note-on and note-off are connected if their pitches are equal.
  match (Note _ px _) (Note _ py _) = px == py
type T = Event Length Point

interpret :: Interpreter E.T (T Bool)
interpret (E.MIDIEvent (C.Cons c (C.Voice v))) = case V.explicitNoteOff v of
  V.NoteOff p vel -> single $ Length False $ Note c p vel
  V.NoteOn p vel -> single $ Length True $ Note c p vel
  _ -> none
interpret (E.MetaEvent (M.TextEvent str)) = single $ Point $ TextEvent str
interpret (E.MetaEvent (M.Lyric str)) = single $ Point $ Lyric str
interpret _ = none

uninterpret :: Uninterpreter (T Bool) E.T
uninterpret evt = (:[]) $ case evt of
  Length b (Note c p v) -> E.MIDIEvent $ C.Cons c $ C.Voice $
    (if b then V.NoteOff else V.NoteOn) p v
  Point p -> E.MetaEvent $ case p of
    TextEvent str -> M.TextEvent str
    Lyric str -> M.Lyric str

standardNote :: V.Pitch -> Length
standardNote p = Note (C.toChannel 0) p (V.toVelocity 96)

-- | Creates a MIDI note of the minimum possible duration allowed by Rock Band.
-- In a valid Rock Band MIDI, this is guaranteed to not overlap other notes.
blip :: V.Pitch -> T Beats
blip p = Length (1 / 32) $ standardNote p

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix xs ys = fmap reverse $ stripPrefix (reverse xs) (reverse ys)
