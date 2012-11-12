module Main where

-- pro guitar to MIDI notes
-- example app using: rhythm, rhythm-midi, rhythm-rockband
-- generates 6 tracks of pitches for the 6 strings of a pro guitar track.

import Data.Rhythm.Event
import Data.Rhythm.Interpret
import Data.Rhythm.Guitar
import qualified Data.Rhythm.MIDI as MIDI
import Data.Rhythm.RockBand.Common
import qualified Data.Rhythm.RockBand.Lex.ProGuitar as PG

import qualified Data.RockBand.MIDIProAdapter as MPA

import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Numeric.NonNegative.Class as NN

import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Sound.MIDI.File.Load as Load
import qualified Sound.MIDI.File.Save as Save

import Control.Applicative
import Data.Maybe
import Data.Char (toUpper)
import System.Environment (getArgs)

playString :: (NN.C t) => Difficulty -> SixString -> SixTuning Int ->
  RTB.T t (PG.T a) -> RTB.T t (MIDI.T a)
playString diff str tun = RTB.mapMaybe f where
  f :: PG.T a -> Maybe (MIDI.T a)
  f (Length len (PG.DiffEvent diff' devt)) | diff == diff' = case devt of
    PG.Note str' frt _ | str == str' -> Just $ Length len $ MIDI.Note ch p v
      where ch = toEnum $ fromEnum str
            p = V.toPitch $ play str frt tun
            v = V.toVelocity 96
    _ -> Nothing
  f _ = Nothing

playStrings :: (NN.C t) => Difficulty -> SixTuning Int -> RTB.T t (PG.T a) ->
  [(Maybe String, RTB.T t (MIDI.T a))]
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
-- TODO: other tunings
readTuning _ = error "readTuning: unrecognized tuning"

run :: MPA.GtrController -> Instrument -> Difficulty -> SixTuning Int ->
  FilePath -> FilePath -> IO ()
run c i d tun fin fout = Load.fromFile fin >>= \f -> case MIDI.readFile f of
  Nothing -> putStrLn "Not a type-1 ticks-based MIDI file"
  Just m  -> let
    unified = MIDI.unifyFile m
    trks = playStrings d tun $ getPG c i unified
    m' = MIDI.splitFile $ unified { MIDI.tracks = trks }
    in case MIDI.showFile m' of
      Nothing -> putStrLn "time signature error"
      Just f' -> Save.toFile fout f'

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

