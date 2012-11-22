{-# LANGUAGE ViewPatterns #-}
{- | The events found in the \"PART VOCALS\", \"HARM1\", \"HARM2\", and
     \"HARM3\" tracks. -}
module Data.Rhythm.RockBand.Lex.Vocals where

import Data.Rhythm.RockBand.Common
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Data.Rhythm.MIDI as MIDI
import Data.Rhythm.Event
import Data.Rhythm.Time
import Data.Rhythm.Interpret
import qualified Numeric.NonNegative.Class as NN
import Data.Char (toLower)
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as M

data Point
  = LyricShift
  | Mood Mood
  | Lyric String
  -- | A playable percussion note.
  | Percussion
  -- | A nonplayable percussion note, which just triggers the sound sample.
  | PercussionSound
  | PercussionAnimation PercussionType Bool
  deriving (Eq, Ord, Show, Read)

data Length
  -- | General phrase marker (RB3) or Player 1 phrases (pre-RB3).
  = Phrase
  -- | Pre-RB3, used for 2nd player phrases in Tug of War.
  | Phrase2
  | Overdrive
  | RangeShift
  -- | Pitches from 36 to 84 are valid.
  | Note V.Pitch
  deriving (Eq, Ord, Show)

instance Long Length where
  match (Note _) (Note _) = True
  match x y = x == y
type T = Event Length Point

data PercussionType
  = Tambourine
  | Cowbell
  | Clap
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

interpret :: (NN.C a) => Interpreter (MIDI.T a) (T a)
interpret (Length len (MIDI.Note _ p _)) = case V.fromPitch p of
  0 -> single $ Length len RangeShift
  1 -> single $ Point LyricShift
  i | 36 <= i && i <= 84 -> single $ Length len $ Note p
  96 -> single $ Point Percussion
  97 -> single $ Point PercussionSound
  105 -> single $ Length len Phrase
  106 -> single $ Length len Phrase2
  116 -> single $ Length len Overdrive
  _ -> none
interpret (Point (E.MetaEvent (M.Lyric str))) = single $ Point $ Lyric str
interpret (Point (E.MetaEvent (M.TextEvent str))) = case str of
  (readPercAnim -> Just evt) -> single $ Point evt
  (readMood     -> Just m  ) -> single $ Point $ Mood m
  _ -> warn w >> single (Point $ Lyric str) where
    w = "Unrecognized text " ++ show str ++ " treated as lyric"
interpret _ = none

uninterpret :: Uninterpreter (T Beats) (MIDI.T Beats)
uninterpret (Point p) = (:[]) $ case p of
  LyricShift -> blip $ V.toPitch 1
  Mood m -> Point . E.MetaEvent . M.TextEvent $ showMood m
  Lyric str -> Point . E.MetaEvent $ M.Lyric str
  Percussion -> blip $ V.toPitch 96
  PercussionSound -> blip $ V.toPitch 97
  PercussionAnimation t b -> Point . E.MetaEvent . M.TextEvent $ showPercAnim t b
uninterpret (Length len l) = (:[]) $ Length len $ standardNote $ case l of
  Overdrive -> V.toPitch 116
  Phrase -> V.toPitch 105
  Phrase2 -> V.toPitch 106
  RangeShift -> V.toPitch 0
  Note p -> p

readPercAnim :: String -> Maybe Point
readPercAnim str = case str of
  "[tambourine_start]" -> f Tambourine True
  "[tambourine_end]" -> f Tambourine False
  "[cowbell_start]" -> f Cowbell True
  "[cowbell_end]" -> f Cowbell False
  "[clap_start]" -> f Clap True
  "[clap_end]" -> f Clap False
  _ -> Nothing
  where f typ b = Just $ PercussionAnimation typ b

showPercAnim :: PercussionType -> Bool -> String
showPercAnim typ b =
  "[" ++ map toLower (show typ) ++ if b then "_start]" else "_end]"
