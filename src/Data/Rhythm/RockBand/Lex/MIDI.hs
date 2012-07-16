-- | A simpler format to hold only the MIDI events used in Rock Band files.
{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Data.Rhythm.RockBand.Lex.MIDI where

import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as M
import qualified Sound.MIDI.Message.Channel as C
import qualified Sound.MIDI.Message.Channel.Voice as V
import Data.Rhythm.Time
import Data.Rhythm.Event
import Data.List (stripPrefix)
import Control.Monad (guard)
import Data.Rhythm.Interpret

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

instance Interpret E.T (T Bool) where
  interpret (E.MIDIEvent (C.Cons c (C.Voice v))) = case V.explicitNoteOff v of
    V.NoteOff p vel -> ok $ Long False $ Note c p vel
    V.NoteOn p vel -> ok $ Long True $ Note c p vel
    _ -> Nothing
  interpret (E.MetaEvent (M.TextEvent str)) = ok $ Point $ TextEvent str
  interpret (E.MetaEvent (M.Lyric str)) = ok $ Point $ Lyric str
  interpret _ = Nothing

instance Interpret (T Bool) E.T where
  interpret evt = ok $ case evt of
    Long b (Note c p v) -> E.MIDIEvent $ C.Cons c $ C.Voice $
      (if b then V.NoteOff else V.NoteOn) p v
    Point p -> E.MetaEvent $ case p of
      TextEvent str -> M.TextEvent str
      Lyric str -> M.Lyric str

standardNote :: V.Pitch -> Long
standardNote p = Note (C.toChannel 0) p (V.toVelocity 96)

-- | Creates a MIDI note of the minimum possible duration allowed by Rock Band.
-- In a valid Rock Band MIDI, this is guaranteed to not overlap other notes.
blip :: V.Pitch -> T Beats
blip p = Long (1 / 32) $ standardNote p

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix xs ys = fmap reverse $ stripPrefix (reverse xs) (reverse ys)
