-- | A format for type 1 (parallel) MIDI files, encoded with beats/ticks, where
-- the first track is reserved for tempo and time signature changes, and track
-- names are stored separately instead of as MIDI events.
{-# LANGUAGE TupleSections #-}
module Data.Rhythm.MIDI where

import qualified Sound.MIDI.File as F
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as M
-- import qualified Sound.MIDI.Message.Channel as C
-- import qualified Sound.MIDI.Message.Channel.Voice as V
import Data.Rhythm.Time
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Numeric.NonNegative.Class as NNC
import Control.Applicative

data StandardMIDI = StandardMIDI
  { tempoTrack :: RTB.T Beats BPM
  , timeSignatures :: SignatureTrack
  , otherMeta :: RTB.T Beats M.T
  , tracks :: [(String, RTB.T Beats E.T)]
  } deriving (Eq, Ord, Show)

beatTracks :: F.T -> Maybe [RTB.T Beats E.T]
beatTracks (F.Cons F.Parallel (F.Ticks tmp) trks) =
  Just $ map (toBeatTrack (fromIntegral tmp)) trks
beatTracks _ = Nothing

fromBeatTracks :: Resolution -> [RTB.T Beats E.T] -> F.T
fromBeatTracks res trks =
  F.Cons F.Parallel (F.Ticks $ fromIntegral res) $ map (toTickTrack res) trks

-- | Looks for a track name event at position 0 in the track. If found, returns
-- the string, and also the track with any initial track name events removed.
getTrackName :: (NNC.C t, Num t) => RTB.T t E.T -> Maybe (String, RTB.T t E.T)
getTrackName rtb = case RTB.viewL rtb of
  Just ((0, evt), rest) -> case evt of
    E.MetaEvent (M.TrackName str) -> Just $ (str ,) $ case getTrackName rest of
      Nothing -> rest
      Just (_, rest') -> rest'
    _ -> fmap (RTB.cons 0 evt) <$> getTrackName rest
  _ -> Nothing
