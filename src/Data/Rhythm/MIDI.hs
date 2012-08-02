-- | A format for type 1 (parallel) MIDI files, encoded with beats/ticks, where
-- the first track is reserved for tempo and time signature changes, and track
-- names are stored separately instead of as MIDI events.
module Data.Rhythm.MIDI where

import qualified Sound.MIDI.File as F
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as M
-- import qualified Sound.MIDI.Message.Channel as C
-- import qualified Sound.MIDI.Message.Channel.Voice as V
import Data.Rhythm.Time
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Numeric.NonNegative.Wrapper as NN
import qualified Numeric.NonNegative.Class as NN
import Control.Applicative
import Data.Maybe (fromMaybe)
import Data.Ratio

data StandardMIDI = StandardMIDI
  { tempoTrack :: RTB.T Beats BPM
  , signatureTrack :: SignatureTrack
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
getTrackName :: (NN.C t, Num t) => RTB.T t E.T -> Maybe (String, RTB.T t E.T)
getTrackName rtb = case RTB.viewL rtb of
  Just ((0, evt), rest) -> case evt of
    E.MetaEvent (M.TrackName str) -> Just
      (str, fromMaybe rest $ snd <$> getTrackName rest)
    _ -> fmap (RTB.cons 0 evt) <$> getTrackName rest
  _ -> Nothing

getTrack :: String -> StandardMIDI -> Maybe (RTB.T Beats E.T)
getTrack str sm = lookup str $ tracks sm

deleteTrack :: String -> StandardMIDI -> StandardMIDI
deleteTrack str sm = sm { tracks = filter (\(x, _) -> x /= str) $ tracks sm }

-- | From a track of meta events, creates a MIDI file with no other tracks.
splitMetaTrack :: RTB.T Beats E.T -> StandardMIDI
splitMetaTrack evts = StandardMIDI tempo (validSignatures tsigs) meta [] where
  (tempo, evts') = RTB.partitionMaybe getBPM evts
  (tsigs, evts'') = RTB.partitionMaybe getTSig evts'
  meta = RTB.mapMaybe getMeta evts''
  getBPM (E.MetaEvent (M.SetTempo mspqn)) = Just bpm
    where bpm = recip $ fromIntegral mspqn / 60000
    -- MIDI tempo is microsecs/quarternote.
  getBPM _ = Nothing
  getTSig (E.MetaEvent (M.TimeSig n d _ _)) =
    Just $ TimeSignature (fromIntegral n) $ 2 ^ (2 - d)
    -- "d" is neg. power of 2. so d=2 means 2^-2 = qnote. d=3 means 2^-3 = 8th.
  getTSig _ = Nothing
  getMeta (E.MetaEvent m) = Just m
  getMeta _ = Nothing

toStandardMIDI :: F.T -> Maybe StandardMIDI
toStandardMIDI f = beatTracks f >>= \trks -> Just $ case trks of
  [] -> StandardMIDI RTB.empty RTB.empty RTB.empty []
  (t:ts) -> (splitMetaTrack t) { tracks = map attachName ts } where
    attachName trk = fromMaybe ("", trk) $ getTrackName trk

fromStandardMIDI :: Resolution -> StandardMIDI -> F.T
fromStandardMIDI res sm = fromBeatTracks res $ allmeta : other where
  allmeta = fmap E.MetaEvent $ RTB.merge tempo $ RTB.merge sigs $ otherMeta sm
  tempo = fmap (\bpm -> M.SetTempo $ floor $ recip bpm * 60000) $ tempoTrack sm
  sigs = fmap makeSig $ renderSignatures $ signatureTrack sm
  makeSig ts = fromMaybe (error sigError) $ toMIDISignature ts
    where sigError = "Time signature not representable in MIDI: " ++ show ts
  other = map (\(name, trk) -> RTB.cons 0 (E.MetaEvent $ M.TrackName name) trk)
    $ tracks sm

toMIDISignature :: TimeSignature -> Maybe M.T
toMIDISignature (TimeSignature mult unit) = isPowerOf2 (NN.toNumber unit) >>=
  \pow -> Just $ M.TimeSig (fromIntegral mult) (2 - fromIntegral pow) 24 8
  where isPowerOf2 :: Rational -> Maybe Integer
        isPowerOf2 r = case (numerator r, denominator r) of
          (1, d) -> negate <$> isPowerOf2' d
          (n, 1) -> isPowerOf2' n
          _ -> Nothing
        isPowerOf2' :: Integer -> Maybe Integer
        isPowerOf2' 1 = Just 0
        isPowerOf2' n = case quotRem n 2 of
          (n', 0) -> (+1) <$> isPowerOf2' n'
          _ -> Nothing
