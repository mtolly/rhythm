{-# LANGUAGE ViewPatterns #-}
-- | The contents of the \"PART REAL_KEYS_?\" and \"KEYS_ANIM_?H\" tracks.
module Data.Rhythm.RockBand.Lex.ProKeys where

import Data.Rhythm.RockBand.Common
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as M
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Data.Rhythm.MIDI as MIDI
import Data.Rhythm.Event
import Data.Rhythm.Time
import qualified Numeric.NonNegative.Class as NN
import Data.Rhythm.Interpret

instance Long Length
type T = Event Length Point

data Point
  {- | Change the viewable play range. Should be placed at least a measure
       before any notes that require the new range. -}
  = LaneShift LaneRange
  -- | The beginning/end of Pro Keys trainer sections.
  | Trainer Trainer
  | Mood Mood
  deriving (Eq, Ord, Show)

data Length
  = Solo -- ^ A keyboard solo section.
  | Glissando -- ^ Place over a sequence of white notes for a freeform section.
  | Trill -- ^ Fill lanes on two keys.
  | Overdrive -- ^ An energy phrase.
  | BRE -- ^ Fill lanes for a Big Rock Ending.
  | Note V.Pitch -- ^ Valid pitches are in MIDI range 48 to 72.
  deriving (Eq, Ord, Show)

-- | There are six playable ranges, each of which covers 10 white keys, plus
-- all the black keys within. They are named here according to their lowest key.
data LaneRange = C | D | E | F | G | A
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

interpret :: (NN.C t) => Interpreter (MIDI.T t) (T t)
interpret (Length len (MIDI.Note _ p _)) = case V.fromPitch p of
  0 -> single $ Point $ LaneShift C
  2 -> single $ Point $ LaneShift D
  4 -> single $ Point $ LaneShift E
  5 -> single $ Point $ LaneShift F
  7 -> single $ Point $ LaneShift G
  9 -> single $ Point $ LaneShift A
  i | 48 <= i && i <= 72 -> single $ Length len $ Note p
  115 -> single $ Length len Solo
  116 -> single $ Length len Overdrive
  120 -> single $ Length len BRE
  126 -> single $ Length len Glissando
  127 -> single $ Length len Trill
  _ -> none
interpret (Point (E.MetaEvent (M.TextEvent str))) = case str of
  (readMood -> Just m) -> single $ Point $ Mood m
  (readTrainer -> Just (t, "key")) -> single $ Point $ Trainer t
  _ -> none
interpret _ = none

uninterpret :: Uninterpreter (T Beats) (MIDI.T Beats)
uninterpret (Point p) = (:[]) $ case p of
  LaneShift rng -> blip $ V.toPitch $ [0, 2, 4, 5, 7, 9] !! fromEnum rng
  Trainer t -> Point . E.MetaEvent . M.TextEvent $ showTrainer t "key"
  Mood m -> Point . E.MetaEvent . M.TextEvent $ showMood m
uninterpret (Length len l) = (:[]) $ Length len $ standardNote $ case l of
  Solo -> V.toPitch 115
  Glissando -> V.toPitch 126
  Trill -> V.toPitch 127
  Overdrive -> V.toPitch 116
  BRE -> V.toPitch 120
  Note p -> p