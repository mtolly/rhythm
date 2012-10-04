{-# LANGUAGE ViewPatterns #-}
-- | The contents of the \"EVENTS\" track.
module Data.Rhythm.RockBand.Lex.Events where

import Control.Monad ((>=>), mplus)
import qualified Data.Rhythm.MIDI as MIDI
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as M
import Data.List (stripPrefix)
import Data.Rhythm.Event
import Data.Rhythm.Interpret
import Data.Rhythm.RockBand.Common

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
  = CrowdRealtime
  | CrowdIntense
  | CrowdNormal
  | CrowdMellow
  | Clap Bool
  deriving (Eq, Ord, Show, Read)

interpret :: Interpreter (MIDI.T t) T
interpret (Point (E.MetaEvent (M.TextEvent str))) = case str of
  (readCrowd -> Just c) -> single $ Crowd c
  (readPractice -> Just sec) -> single $ Practice sec
  "[music_start]" -> single MusicStart
  "[music_end]"   -> single MusicEnd
  "[end]"         -> single End
  "[coda]"        -> single Coda
  _               -> none
interpret _ = none

readCrowd :: String -> Maybe Crowd
readCrowd = stripPrefix "[crowd_" >=> \rest -> case rest of
  "realtime]" -> Just CrowdRealtime
  "intense]"  -> Just CrowdIntense
  "normal]"   -> Just CrowdNormal
  "mellow]"   -> Just CrowdMellow
  "clap]"     -> Just $ Clap True
  "noclap]"   -> Just $ Clap False
  _           -> Nothing

-- | Supports both pre-RB3 (\"section\") and RB3 (\"prc\") event formats.
readPractice :: String -> Maybe String
readPractice s = mplus (stripPrefix "[prc_" s) (stripPrefix "[section " s)
  >>= stripSuffix "]"
