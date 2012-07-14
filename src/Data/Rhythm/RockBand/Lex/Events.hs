{-# LANGUAGE ViewPatterns #-}
-- | The contents of the \"EVENTS\" track.
module Data.Rhythm.RockBand.Lex.Events where

import Control.Monad ((>=>), mplus)
import qualified Data.Rhythm.RockBand.Lex.MIDI as MIDI
import Data.List (stripPrefix)
import Data.Rhythm.Event

data T
  = MusicStart
  | MusicEnd
  | End
  | Coda
  | Crowd Crowd
  -- | The string is something like \"verse_1\" or \"gtr_solo_2a\".
  | Practice String
  deriving (Eq, Ord, Show, Read)

data Crowd
  = Realtime
  | Intense
  | Normal
  | Mellow
  | Clap Bool
  deriving (Eq, Ord, Show, Read)

readEvent :: MIDI.T a -> Maybe T
readEvent (Point (MIDI.TextEvent str)) = case str of
  (readCrowd -> Just c) -> Just $ Crowd c
  (readPractice -> Just sec) -> Just $ Practice sec
  "[music_start]" -> Just MusicStart
  "[music_end]"   -> Just MusicEnd
  "[end]"         -> Just End
  "[coda]"        -> Just Coda
  _               -> Nothing
readEvent _ = Nothing

readCrowd :: String -> Maybe Crowd
readCrowd = stripPrefix "[crowd_" >=> \rest -> case rest of
  "realtime]" -> Just Realtime
  "intense]"  -> Just Intense
  "normal]"   -> Just Normal
  "mellow]"   -> Just Mellow
  "clap]"     -> Just $ Clap True
  "noclap]"   -> Just $ Clap False
  _           -> Nothing

-- | Supports both pre-RB3 (\"section\") and RB3 (\"prc\") event formats.
readPractice :: String -> Maybe String
readPractice s = mplus (stripPrefix "[prc_" s) (stripPrefix "[section " s)
  >>= MIDI.stripSuffix "]"
