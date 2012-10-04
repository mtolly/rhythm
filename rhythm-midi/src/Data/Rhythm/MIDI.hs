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
import Data.Traversable (traverse)
import qualified Data.Rhythm.Status as Status

data File t a = File
  -- | Ticks per beat for storing the MIDI file.
  { resolution :: Resolution
  -- | The tempo change events from the first MIDI track.
  , tempoTrack :: Status.T t BPM
  -- | The time signature events from the first MIDI track.
  , signatureTrack :: SignatureTrack
  -- | Other events (not tempos or time signatures) from the first MIDI track.
  , trackZero :: RTB.T t (T a)
  -- | The parallel tracks of the type-1 MIDI file.
  , tracks :: [(Maybe String, RTB.T t (T a))]
  } deriving (Eq, Ord, Show)

-- | It is an error to make a Point which holds a NoteOn or NoteOff.
-- It is also an error to make a "Length True (NoteOff _ _ 0)".
type T = Event Note E.T

data Note = Note C.Channel V.Pitch V.Velocity
  deriving (Eq, Ord, Show)

instance Long Note where
  match (Note c0 p0 _) (Note c1 p1 _) = c0 == c1 && p0 == p1

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

unifyFile :: (NN.C t) => File t Bool -> File t t
unifyFile m = File
  { resolution = resolution m
  , tempoTrack = tempoTrack m
  , signatureTrack = signatureTrack m
  , trackZero = unifyEvents $ trackZero m
  , tracks = map (fmap unifyEvents) $ tracks m }

splitFile :: (NN.C t) => File t t -> File t Bool
splitFile m = File
  { resolution = resolution m
  , tempoTrack = tempoTrack m
  , signatureTrack = signatureTrack m
  , trackZero = splitEvents $ trackZero m
  , tracks = map (fmap splitEvents) $ tracks m }

fromTickFile :: File Ticks a -> File Beats a
fromTickFile f = File
  { resolution = res
  , tempoTrack = fromTickStatus res $ tempoTrack f
  , signatureTrack = signatureTrack f
  , trackZero = fromTickTrack res $ trackZero f
  , tracks = map (fmap $ fromTickTrack res) $ tracks f
  } where res = resolution f

toTickFile :: File Beats a -> File Ticks a
toTickFile f = File
  { resolution = res
  , tempoTrack = toTickStatus res $ tempoTrack f
  , signatureTrack = signatureTrack f
  , trackZero = toTickTrack res $ trackZero f
  , tracks = map (fmap $ toTickTrack res) $ tracks f
  } where res = resolution f

getTempo :: T a -> Maybe BPM
getTempo (Point (E.MetaEvent (M.SetTempo mspqn))) = Just bpm
  where bpm = 60000 / fromIntegral mspqn
  -- MIDI tempo is microsecs/quarternote.
getTempo _ = Nothing

getSignature :: T a -> Maybe TimeSignature
getSignature (Point (E.MetaEvent (M.TimeSig n d _ _))) =
  Just $ TimeSignature (fromIntegral n) $ 2 ^^ (2 - d)
getSignature _ = Nothing

readFile :: F.T -> Maybe (File Ticks Bool)
readFile (F.Cons F.Parallel (F.Ticks res) trks) = Just $
  case map (fmap readEvent) trks of
    [] -> File (fromIntegral res) (Status.Stay 120) (Status.Stay $ 4 // 4)
      RTB.empty []
    meta : rest -> File res' stempo ssig tzero rest' where
      res' = fromIntegral res
      (ttempo, meta') = RTB.partitionMaybe getTempo meta
      (tsig, tzero) = RTB.partitionMaybe getSignature meta'
      stempo = Status.fromRTB 120 ttempo
      ssig = validSignatures $ Status.fromRTB (4 // 4) $ fromTickTrack res' tsig
      rest' = map extractName rest
readFile _ = Nothing

makeTempo :: BPM -> T a
makeTempo bpm = Point $ E.MetaEvent $ M.SetTempo $ floor $ 60000 / bpm

makeSignature :: TimeSignature -> Maybe (T a)
makeSignature (TimeSignature mult unit) = isPowerOf2 (NN.toNumber unit) >>=
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

showFile :: File Ticks Bool -> Maybe F.T
showFile m = let
  res = resolution m
  tempo = Status.toRTB' $ Status.cleanRedundant $ makeTempo <$> tempoTrack m
    :: RTB.T Ticks (T Bool)
  mbsigs = traverse makeSignature $ toTickTrack res $ Status.toRTB' $
    Status.cleanRedundant $ renderSignatures $ signatureTrack m
    :: Maybe (RTB.T Ticks (T Bool))
  in mbsigs >>= \sigs -> undefined
  -- restTracks = map attachName $ tracks m
  -- attachName (Nothing, trk) = trk
  -- attachName (Just name, trk) = RTB.cons 0 (Point $ E.MetaEvent $ M.TrackName name) trk
  -- maybeSigs = traverse toMIDISignature $ renderSignatures $ signatureTrack m
  -- in maybeSigs >>= \sigs -> return $ let
    -- allMeta = RTB.merge tempo $ RTB.merge (Status.toRTB $ fmap (Point . E.MetaEvent) sigs) $ trackZero m
    -- allTracks = map (fmap showEvent) $ allMeta : restTracks
    -- in F.Cons F.Parallel (F.Ticks $ fromIntegral res) allTracks

extractName :: (NN.C t) => RTB.T t (T a) -> (Maybe String, RTB.T t (T a))
extractName rtb = case RTB.viewL rtb of
  Just ((dt, evt), rest)
    | dt == NN.zero -> case evt of
      Point (E.MetaEvent (M.TrackName s)) -> (Just s, snd $ extractName rest)
      _ -> fmap (RTB.cons NN.zero evt) $ extractName rest
    | otherwise -> (Nothing, rtb)
  _ -> (Nothing, RTB.empty)

getTrack :: String -> File t a -> Maybe (RTB.T t (T a))
getTrack str = lookup (Just str) . tracks

deleteTrack :: String -> File t a -> File t a
deleteTrack str m = m { tracks = filter (\(x, _) -> x /= Just str) $ tracks m }
