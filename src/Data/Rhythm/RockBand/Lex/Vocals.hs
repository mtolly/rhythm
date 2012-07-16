{-# LANGUAGE ViewPatterns, MultiParamTypeClasses,
    TypeSynonymInstances, FlexibleInstances #-}
{- | The events found in the \"PART VOCALS\", \"HARM1\", \"HARM2\", and
     \"HARM3\" tracks. -}
module Data.Rhythm.RockBand.Lex.Vocals where

import Data.Rhythm.RockBand.Common
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Data.Rhythm.RockBand.Lex.MIDI as MIDI
import Data.Rhythm.Event
import Data.Rhythm.Time
import Data.Rhythm.Interpret
import qualified Numeric.NonNegative.Class as NNC
import Data.Char (toLower)

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

data Long
  -- | General phrase marker (RB3) or Player 1 phrases (pre-RB3).
  = Phrase
  -- | Pre-RB3, used for 2nd player phrases in Tug of War.
  | Phrase2
  | Overdrive
  | RangeShift
  -- | Pitches from 36 to 84 are valid.
  | Note V.Pitch
  deriving (Eq, Ord, Show)

instance Duration Long Point
type T = Event Long Point

data PercussionType
  = Tambourine
  | Cowbell
  | Clap
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance (NNC.C a) => Interpret (MIDI.T a) (T a) where
  interpret (Long len (MIDI.Note _ p _)) = case V.fromPitch p of
    0 -> ok $ Long len RangeShift
    1 -> ok $ Point LyricShift
    i | 36 <= i && i <= 84 -> ok $ Long len $ Note p
    96 -> ok $ Point Percussion
    97 -> ok $ Point PercussionSound
    105 -> ok $ Long len Phrase
    106 -> ok $ Long len Phrase2
    116 -> ok $ Long len Overdrive
    _ -> Nothing
  interpret (Point (MIDI.Lyric str)) = ok $ Point $ Lyric str
  interpret (Point (MIDI.TextEvent str)) = case str of
    (readPercAnim -> Just evt) -> ok $ Point evt
    (readMood     -> Just m  ) -> ok $ Point $ Mood m
    _ -> Just ([Point $ Lyric str], [warning]) where
      warning = "Unrecognized text \"" ++ show str ++ "\" treated as lyric"

instance Interpret (T Beats) (MIDI.T Beats) where
  interpret (Point p) = ok $ case p of
    LyricShift -> MIDI.blip $ V.toPitch 1
    Mood m -> Point $ MIDI.TextEvent $ showMood m
    Lyric str -> Point $ MIDI.Lyric str
    Percussion -> MIDI.blip $ V.toPitch 96
    PercussionSound -> MIDI.blip $ V.toPitch 97
    PercussionAnimation t b -> Point $ MIDI.TextEvent $ showPercAnim t b
  interpret (Long len l) = ok $ Long len $ MIDI.standardNote $ case l of
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
