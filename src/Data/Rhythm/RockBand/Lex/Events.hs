{-# LANGUAGE ViewPatterns #-}
-- | The contents of the \"EVENTS\" track.
module Data.Rhythm.RockBand.Lex.Events where

import Control.Monad ((>=>), mplus)
import qualified Data.Rhythm.MIDI as MIDI
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as M
import Data.List (stripPrefix)
import Data.Rhythm.Event
import Data.Rhythm.RockBand.Common
import Data.Rhythm.Parser

data T
  = MusicStart
  | MusicEnd
  | End
  | Coda
  | Crowd Crowd
  | Practice String -- ^ string is something like \"verse_1\" or \"gtr_solo_2a\"
  deriving (Eq, Ord, Show, Read)

data Crowd
  = CrowdRealtime
  | CrowdIntense
  | CrowdNormal
  | CrowdMellow
  | Clap Bool
  deriving (Eq, Ord, Show, Read)

parse :: (Show a) => Parser (MIDI.T a) T
parse = get >>= \x -> case x of
  Point (E.MetaEvent (M.TextEvent str)) -> case str of
    (readCrowd -> Just c) -> return $ Crowd c
    (readPractice -> Just sec) -> return $ Practice sec
    "[music_start]" -> return MusicStart
    "[music_end]"   -> return MusicEnd
    "[end]"         -> return End
    "[coda]"        -> return Coda
    _               -> unrecognized x
  _ -> unrecognized x

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
