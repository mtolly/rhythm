{-# LANGUAGE ViewPatterns #-}
-- | The contents of the \"EVENTS\" track.
module Data.Rhythm.RockBand.Lexer.Events where

import Control.Monad ((>=>))
import qualified Data.Rhythm.RockBand.Lexer.MIDI as MIDI
import Data.List (stripPrefix)
import Data.Rhythm.Types

data Event
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

readEvent :: MIDI.Event a -> Maybe Event
readEvent (Point (MIDI.TextEvent str)) = case str of
  (tryCrowd -> Just c) -> Just $ Crowd c
  (tryPractice -> Just sec) -> Just $ Practice sec
  "[music_start]" -> Just MusicStart
  "[music_end]"   -> Just MusicEnd
  "[end]"         -> Just End
  "[coda]"        -> Just Coda
  _               -> Nothing
readEvent _ = Nothing

tryCrowd :: String -> Maybe Crowd
tryCrowd = stripPrefix "[crowd_" >=> \rest -> case rest of
  "realtime]" -> Just Realtime
  "intense]"  -> Just Intense
  "normal]"   -> Just Normal
  "mellow]"   -> Just Mellow
  "clap]"     -> Just $ Clap True
  "noclap]"   -> Just $ Clap False
  _           -> Nothing

tryPractice :: String -> Maybe String
tryPractice = stripPrefix "[prc_" >=> MIDI.stripSuffix "]"
