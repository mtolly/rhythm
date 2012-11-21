{- |
Converts a Pro Guitar chart to listenable MIDI notes. Requires the rhythm,
rhythm-midi, rhythm-rockband, and midiproadapter packages. The generated MIDI
file will have 6 tracks (on 6 channels) representing the 6 guitar strings.
-}
module Main where

import Data.Rhythm.Event
import Data.Rhythm.Interpret
import Data.Rhythm.Guitar
import Data.Rhythm.Time
import qualified Data.Rhythm.MIDI as MIDI
import Data.Rhythm.RockBand.Common
import qualified Data.Rhythm.RockBand.Lex.ProGuitar as PG

import qualified Data.RockBand.MIDIProAdapter as MPA

import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Rhythm.EventList as RTB
import qualified Numeric.NonNegative.Class as NN

import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.Message.Channel as C
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Sound.MIDI.File.Load as Load
import qualified Sound.MIDI.File.Save as Save

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.Char (toUpper)
import System.Environment (getArgs)
import Control.Monad.Fix (fix)

playString :: Difficulty -> SixString -> SixTuning Int ->
  RTB.T Seconds (PG.T Seconds) -> RTB.T Seconds (MIDI.T Seconds)
playString diff str tun = RTB.join . fmap f where
  f :: PG.T Seconds -> RTB.T Seconds (MIDI.T Seconds)
  f (Length len (PG.DiffEvent diff' devt)) | diff == diff' = case devt of
    PG.Note str' frt typ | str == str' -> case typ of
      PG.ArpeggioForm -> RTB.empty
      PG.Bent -> RTB.merge note $ RTB.take len $ bender (1 / 4) ch
      _ -> RTB.cons 0 (resetPB ch) note
      where ch = toEnum $ fromEnum str
            p = V.toPitch $ play str frt tun
            vel = V.toVelocity 96
            note = RTB.singleton 0 $ Length len $ MIDI.Note ch p vel
    _ -> RTB.empty
  f _ = RTB.empty

-- | An infinite event-list of pitch bend events, oscillating between no bend
-- and maximum bend up. The argument is the period (time of a full cycle).
bender :: (Ord a) => Seconds -> C.Channel -> RTB.T Seconds (MIDI.T a)
bender secs ch = RTB.merge (RTB.singleton 0 $ resetPB ch) $ fix attachRTB where
  attachRTB = foldr (.) id $ map (RTB.cons eventGap) bendEvents
  eventGap = secs / fromIntegral (length bendEvents)
  bendEvents = map
    (Point . E.MIDIEvent . C.Cons ch . C.Voice . V.PitchBend . floor)
    bendValues
  bendValues = map (\n -> fromIntegral n * 8191 / 20 + 8192)
    ([0 .. 20] ++ [19, 18 .. 1] :: [Int])
    :: [Rational]

resetPB :: C.Channel -> MIDI.T a
resetPB ch = Point $ E.MIDIEvent $ C.Cons ch $ C.Voice $ V.PitchBend 8192

playStrings :: Difficulty -> SixTuning Int -> RTB.T Seconds (PG.T Seconds) ->
  [(Maybe String, RTB.T Seconds (MIDI.T Seconds))]
playStrings diff tun pg = zipWith f [S6 .. S1] [6, 5 .. 1] where
  f str n = (Just $ "String " ++ show (n :: Int), playString diff str tun pg)

data Instrument = Guitar | Bass
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

getPG :: (NN.C t) =>
  MPA.GtrController -> Instrument -> MIDI.File t t -> RTB.T t (PG.T t)
getPG c i mid = go $ fromMaybe (error "MIDI track not found") $ case c of
  MPA.Squier  -> MIDI.getTrack name22 mid <|> MIDI.getTrack name17 mid
  MPA.Mustang -> MIDI.getTrack name17 mid
  where go t = fst3 $ interpretRTB PG.interpret t
        fst3 (x, _, _) = x
        name17 = "PART REAL_" ++ map toUpper (show i)
        name22 = name17 ++ "_22"

readTuning :: String -> SixTuning Int
readTuning "Standard" = stdTuning
readTuning "DropD" = dropD
readTuning s = case reads s of
  [(offsets, _)] -> \str -> stdTuning str + (offsets !! fromEnum str)
  _ -> error "readTuning: unrecognized tuning"

run :: MPA.GtrController -> Instrument -> Difficulty -> SixTuning Int ->
  FilePath -> FilePath -> IO ()
run c i d tun fin fout = Load.fromFile fin >>= \f -> case MIDI.readFile f of
  Nothing -> putStrLn "Not a type-1 ticks-based MIDI file"
  Just m  -> let
    unifiedTime = MIDI.unifyFile $ MIDI.toTimeFile $ MIDI.fromTickFile m
      :: MIDI.File Seconds Seconds
    trks = playStrings d tun $ getPG c i unifiedTime
    m' = MIDI.toTickFile $ MIDI.fromTimeFile $ MIDI.splitFile $
      unifiedTime { MIDI.tracks = trks }
    in Save.toFile fout $ MIDI.showFile m'

main :: IO ()
main = getArgs >>= \argv -> case argv of
  [c, i, d, tun, fin, fout] ->
    run (read c) (read i) (read d) (readTuning tun) fin fout
  _ -> mapM_ putStrLn
    [ "Usage: pgtonotes cont inst diff tune file-in file-out"
    , "cont is one of: Squier, Mustang"
    , "inst is one of: Guitar, Bass"
    , "diff is one of: Easy, Medium, Hard, Expert"
    , "tune is one of: Standard, DropD, or offsets from standard tuning"
    , "  in the form: \"[-2, 0, 0, 0, 0, 0]\" (meaning, drop D)"
    ]
