module Data.Rhythm.MIDI where

import qualified Sound.MIDI.File as F
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as M
import qualified Sound.MIDI.Message.Channel as C
import qualified Sound.MIDI.Message.Channel.Voice as V
import Data.Rhythm.Time
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Numeric.NonNegative.Class as NNC

getResolution :: F.T -> Maybe Resolution
getResolution (F.Cons _ (F.Ticks tmp) _) = Just $ fromIntegral tmp
getResolution _ = Nothing

beatTracks :: F.T -> Maybe [RTB.T Beats E.T]
beatTracks f = getResolution f >>= \res ->
  Just $ map (toBeatTrack res) $ F.getTracks f

fromBeatTracks :: F.Type -> Resolution -> [RTB.T Beats E.T] -> F.T
fromBeatTracks typ res trks = F.Cons typ (F.Ticks $ fromIntegral res) $
  map (toTickTrack res) trks

getTrackName :: (NNC.C t, Num t) => RTB.T t E.T -> Maybe String
getTrackName rtb = case RTB.viewL rtb of
  Just ((0, evt), rest) -> case evt of
    E.MetaEvent (M.TrackName str) -> Just str
    _ -> getTrackName rest
  _ -> Nothing
