{-# LANGUAGE PatternGuards #-}
-- | The contents of the \"PART REAL_GUITAR\", \"PART REAL_GUITAR_22\",
-- \"PART REAL_BASS\", and \"PART REAL_BASS_22\" tracks.
module Data.Rhythm.RockBand.Lex.ProGuitar where

import Data.Rhythm.RockBand.Common
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as M
import qualified Sound.MIDI.Message.Channel as C
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Data.Rhythm.MIDI as MIDI
import Data.Rhythm.Time
import Data.Rhythm.Event
import Data.Rhythm.Parser
import qualified Numeric.NonNegative.Class as NN
import Data.Char (isSpace)
import Data.List (stripPrefix, sort)
import Data.Maybe (listToMaybe)
import qualified Data.EventList.Relative.TimeBody as RTB
import Data.Rhythm.Guitar

instance Long Length where
  match (DiffEvent d0 (Slide _)) (DiffEvent d1 (Slide _)) = d0 == d1
  match (DiffEvent d0 (PartialChord _)) (DiffEvent d1 (PartialChord _)) =
    d0 == d1
  match (DiffEvent d0 (UnknownBFlat _ _)) (DiffEvent d1 (UnknownBFlat _ _)) =
    d0 == d1
  match x y = x == y
type T = Event Length Point

data Point
  = TrainerGtr Trainer
  | TrainerBass Trainer
  | HandPosition GtrFret
  | ChordRoot V.Pitch -- ^ Valid pitches are 4 (E) to 15 (D#).
  | ChordName Difficulty String
  deriving (Eq, Ord, Show)

data Length
  = Trill
  | Tremolo
  | BRE
  | Overdrive
  | Solo
  | NoChordNames
  | SlashChords
  | BlackChordSwitch -- ^ I *think* this swaps between (e.g.) F# and Gb chords.
  | DiffEvent Difficulty DiffEvent
  deriving (Eq, Ord, Show)

data DiffEvent
  = Note SixString GtrFret NoteType
  | ForceHOPO
  | Slide SlideType
  | Arpeggio
  | PartialChord StrumArea
  | UnknownBFlat C.Channel V.Velocity
  | AllFrets
  deriving (Eq, Ord, Show)

data NoteType
  = NormalNote
  | ArpeggioForm
  | Bent
  | Muted
  | Tapped
  | Harmonic
  | PinchHarmonic
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data SlideType = NormalSlide | ReversedSlide
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data StrumArea = High | Mid | Low
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

parse :: (NN.C a) => Parser (MIDI.T a) (Maybe (T a))
parse = get >>= \x -> case x of
  Length len n@(MIDI.Note ch p vel) -> case V.fromPitch p of
    i | 4 <= i && i <= 15 -> single $ Point $ ChordRoot p
    16 -> single $ Length len SlashChords
    17 -> single $ Length len NoChordNames
    18 -> single $ Length len BlackChordSwitch
    i | let (oct, k) = quotRem i 12
      , elem oct [2,4,6,8]
      , let makeDiff = single . Length len . DiffEvent (toEnum $ quot oct 2 - 1)
      -> case k of
        6 -> makeDiff ForceHOPO
        7 -> case C.fromChannel ch of
          0   -> makeDiff $ Slide NormalSlide
          11  -> makeDiff $ Slide ReversedSlide
          ch' -> warn w >> makeDiff (Slide NormalSlide) where
            w = "Slide marker (pitch " ++ show i ++
              ") has unknown channel " ++ show ch'
        8 -> makeDiff Arpeggio
        9 -> case C.fromChannel ch of
          13 -> makeDiff $ PartialChord High
          14 -> makeDiff $ PartialChord Mid
          15 -> makeDiff $ PartialChord Low
          ch'  -> warn w >> makeDiff (PartialChord Mid) where
            w = "Partial chord marker (pitch " ++ show i ++
              ") has unknown channel " ++ show ch'
        10 -> makeDiff $ UnknownBFlat ch vel
        11 -> makeDiff AllFrets
        _ -> makeDiff $ Note nstr nfret ntyp where
          nstr = toEnum k
          nfret = fromIntegral $ V.fromVelocity vel - 100
          ntyp = toEnum $ C.fromChannel ch
    108 -> single $ Point $ HandPosition $ fromIntegral $ V.fromVelocity vel - 100
    115 -> single $ Length len Solo
    116 -> single $ Length len Overdrive
    120 -> single $ Length len BRE
    121 -> return Nothing
    122 -> return Nothing
    123 -> return Nothing
    124 -> return Nothing
    125 -> return Nothing
    126 -> single $ Length len Tremolo
    127 -> single $ Length len Trill
    _ -> unrecognized n
  Point (E.MetaEvent txt@(M.TextEvent str)) -> case readTrainer str of
    Just (t, "pg") -> single $ Point $ TrainerGtr t
    Just (t, "pb") -> single $ Point $ TrainerBass t
    _ -> case readChordName str of
      Nothing -> unrecognized txt
      Just (diff, name) -> single $ Point $ ChordName diff name
  Point p -> unrecognized p
  where single = return . Just

unparse :: T Beats -> [MIDI.T Beats]
unparse (Point x) = case x of
  TrainerGtr t -> [text $ showTrainer t "pg"]
  TrainerBass t -> [text $ showTrainer t "pb"]
  HandPosition f -> [blip $ V.toPitch $ fromIntegral f + 100]
  ChordRoot p -> [blip p]
  ChordName diff str -> [text $ showChordName diff str]
  where text = Point . E.MetaEvent . M.TextEvent
unparse (Length len x) = case x of
  SlashChords -> [note 16]
  NoChordNames -> [note 17]
  BlackChordSwitch -> [note 18]
  Solo -> [note 115]
  Overdrive -> [note 116]
  BRE -> map note [120 .. 125]
  Tremolo -> [note 126]
  Trill -> [note 127]
  DiffEvent diff evt -> [Length len $ MIDI.Note ch p vel] where
    ch = case evt of
      Note _ _ typ -> toEnum $ fromEnum typ
      Slide typ -> toEnum $ fromEnum typ * 11
      PartialChord area -> toEnum $ fromEnum area + 13
      UnknownBFlat c _ -> c
      _ -> toEnum 0
    p = V.toPitch $ diffOffset + case evt of
      Note str _ _ -> fromEnum str
      ForceHOPO -> 6
      Slide _ -> 7
      Arpeggio -> 8
      PartialChord _ -> 9
      UnknownBFlat _ _ -> 10
      AllFrets -> 11
    vel = case evt of
      Note _ frt _ -> V.toVelocity $ fromIntegral frt + 100
      UnknownBFlat _ v -> v
      _ -> V.toVelocity 96
    diffOffset = (fromEnum diff + 1) * 24
  where note = Length len . standardNote . V.toPitch

readChordName :: String -> Maybe (Difficulty, String)
readChordName str
  | Just (x:xs) <- stripPrefix "[chrd" str
  , elem x "0123"
  , let diff = toEnum $ read [x]
  , Just name <- stripSuffix "]" $ dropWhile isSpace xs
  = Just (diff, name)
  | otherwise = Nothing

showChordName :: Difficulty -> String -> String
showChordName d ch = "[chrd" ++ show (fromEnum d) ++ " " ++ ch ++ "]"

-- | Generates a fret number for each note or chord.
autoHandPosition :: (NN.C t) => RTB.T t DiffEvent -> RTB.T t GtrFret
autoHandPosition = RTB.mapMaybe getFret . RTB.collectCoincident where
  getFret :: [DiffEvent] -> Maybe GtrFret
  getFret evts = listToMaybe $ sort
    [f | Note _ f _ <- evts, f /= 0]
