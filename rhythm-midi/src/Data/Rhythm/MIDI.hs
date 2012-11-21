-- | A format for type 1 (parallel) MIDI files, encoded with beats/ticks, where
-- the first track is reserved for tempo and time signature changes, and track
-- names are stored separately instead of as MIDI events.
module Data.Rhythm.MIDI where

import qualified Sound.MIDI.File as F
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as M
import qualified Sound.MIDI.Message.Channel as C
import qualified Sound.MIDI.Message.Channel.Voice as V
import Data.Rhythm.Time
import Data.Rhythm.TimeSignature
import Data.Rhythm.Event
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Numeric.NonNegative.Wrapper as NN
import qualified Numeric.NonNegative.Class as NN
import Control.Applicative
import Data.Ratio
import qualified Data.Rhythm.Status as Status

data File t a = File
  { resolution :: Resolution
  -- ^ Ticks per beat for storing the MIDI file.
  , trackZero :: RTB.T t (T a)
  -- ^ The first MIDI track, which holds tempo and time signature changes.
  , tracks :: [(Maybe String, RTB.T t (T a))]
  -- ^ The parallel tracks in a type-1 MIDI file, along with track names.
  } deriving (Eq, Ord, Show)

-- | It is an error to make a Point which holds a NoteOn or NoteOff.
-- It is also an error to make a "Length True (NoteOff _ _ 0)".
type T = Event Note E.T

data Note = Note C.Channel V.Pitch V.Velocity
  deriving (Eq, Ord, Show)

instance Long Note where
  match (Note c0 p0 _) (Note c1 p1 _) = c0 == c1 && p0 == p1

tempoTrack :: (NN.C t) => File t a -> Status.T t BPM
tempoTrack = Status.fromRTB (Beats 120) . RTB.mapMaybe getTempo . trackZero

signatureTrack :: File Beats a -> SignatureTrack
signatureTrack = validSignatures . Status.fromRTB (4 // 4) .
  RTB.mapMaybe getSignature . trackZero

readEvent :: E.T -> T Bool
readEvent (E.MIDIEvent (C.Cons ch (C.Voice (V.NoteOff p v)))) =
  Length False $ Note ch p v
readEvent (E.MIDIEvent (C.Cons ch (C.Voice (V.NoteOn p v)))) =
  Length (V.fromVelocity v /= 0) $ Note ch p v
readEvent evt = Point evt

showEvent :: T Bool -> E.T
showEvent (Length b (Note ch p v)) =
  E.MIDIEvent $ C.Cons ch $ C.Voice $ (if b then V.NoteOn else V.NoteOff) p v
showEvent (Point evt) = evt

-- | Modifies a file by applying functions to each track.
mapTracks :: (RTB.T t (T a) -> RTB.T u (T b)) -> File t a -> File u b
mapTracks frtb m = File
  { resolution = resolution m
  , trackZero = frtb $ trackZero m
  , tracks = map (mapSnd frtb) $ tracks m }
  where mapSnd = fmap

unifyFile :: (NN.C t) => File t Bool -> File t t
unifyFile = mapTracks unifyEvents

splitFile :: (NN.C t) => File t t -> File t Bool
splitFile = mapTracks splitEvents

-- | Uses the file's tempo track to convert.
toTimeFile :: File Beats a -> File Seconds a
toTimeFile m = mapTracks (toTimeTrack $ tempoTrack m) m

-- | Uses the file's tempo track to convert.
fromTimeFile :: File Seconds a -> File Beats a
fromTimeFile m = mapTracks (fromTimeTrack $ tempoTrack m) m

-- | Uses the file's resolution to convert.
fromTickFile :: File Ticks a -> File Beats a
fromTickFile m = mapTracks (fromTickTrack $ resolution m) m

-- | Uses the file's resolution to convert.
toTickFile :: File Beats a -> File Ticks a
toTickFile m = mapTracks (toTickTrack $ resolution m) m

-- | Decodes a tempo from a MIDI event.
getTempo :: T a -> Maybe BPM
getTempo (Point (E.MetaEvent (M.SetTempo microsecsPerBeat))) = Just beatsPerMin
  where beatsPerMin = microsecsPerMin / fromIntegral microsecsPerBeat
        microsecsPerMin = 60000000
getTempo _ = Nothing

-- | Decodes a time signature from a MIDI event.
getSignature :: T a -> Maybe TimeSignature
getSignature (Point (E.MetaEvent (M.TimeSig n d _ _))) =
  Just $ TimeSignature (fromIntegral n) $ 2 ^^ (2 - d)
getSignature _ = Nothing

readFile :: F.T -> Maybe (File Ticks Bool)
readFile (F.Cons F.Parallel (F.Ticks res) trks) = Just $
  case map (fmap readEvent . RTB.mapTime Ticks) trks of
    [] -> File res' RTB.empty []
    meta : rest -> File res' meta $ map extractName rest where
    where res' = fromIntegral res
readFile _ = Nothing

-- | Encodes a tempo as a MIDI event.
makeTempo :: BPM -> T a
makeTempo beatsPerMin = Point $ E.MetaEvent $ M.SetTempo microsecsPerBeat
  where microsecsPerBeat = floor $ microsecsPerMin / beatsPerMin
        microsecsPerMin = 60000000

-- | Encodes a time signature as a MIDI event, or Nothing if the denominator of
-- the signature isn't a power of 2.
makeSignature :: TimeSignature -> Maybe (T a)
makeSignature (TimeSignature mult (Beats unit)) = isPowerOf2 (NN.toNumber unit) >>=
  \pow -> Just $ Point $ E.MetaEvent $ M.TimeSig (fromIntegral mult)
    (2 - fromIntegral pow) 24 8 where
    isPowerOf2 :: Rational -> Maybe Integer
    isPowerOf2 r = case (numerator r, denominator r) of
      (1, d) -> negate <$> isPowerOf2' d
      (n, 1) -> isPowerOf2' n
      _ -> Nothing
    isPowerOf2' :: Integer -> Maybe Integer
    isPowerOf2' 1 = Just 0
    isPowerOf2' n = case quotRem n 2 of
      (n', 0) -> (+1) <$> isPowerOf2' n'
      _ -> Nothing

showFile :: File Ticks Bool -> F.T
showFile f = F.Cons F.Parallel (F.Ticks $ fromIntegral $ resolution f) $
  map (fmap showEvent . RTB.mapTime unTicks) $
  trackZero f : map (uncurry attachName) (tracks f)

extractName :: (NN.C t) => RTB.T t (T a) -> (Maybe String, RTB.T t (T a))
extractName rtb = case RTB.viewL rtb of
  Just ((dt, evt), rest)
    | dt == NN.zero -> case evt of
      Point (E.MetaEvent (M.TrackName s)) -> (Just s, snd $ extractName rest)
      _ -> fmap (RTB.cons NN.zero evt) $ extractName rest
    | otherwise -> (Nothing, rtb)
  _ -> (Nothing, RTB.empty)

attachName :: (NN.C t) => Maybe String -> RTB.T t (T a) -> RTB.T t (T a)
attachName = maybe id $ \s ->
  RTB.cons NN.zero $ Point $ E.MetaEvent $ M.TrackName s

getTrack :: String -> File t a -> Maybe (RTB.T t (T a))
getTrack str = lookup (Just str) . tracks

deleteTrack :: String -> File t a -> File t a
deleteTrack str m = m { tracks = filter (\(x, _) -> x /= Just str) $ tracks m }
