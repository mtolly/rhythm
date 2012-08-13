module Main where

-- pro guitar autoplayer
-- example app using rhythm and midiproadapter libraries

import qualified Data.Rhythm.RockBand.Lex.ProGuitar as PG
import Data.Rhythm.RockBand.Common

import Data.Rhythm.Event
import Data.Rhythm.Interpret
import Data.Rhythm.Time

import qualified Data.RockBand.MIDIProAdapter as MPA

import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Numeric.NonNegative.Class as NN

import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.SystemExclusive as SysEx
import qualified Sound.MIDI.File.Load as Load
import qualified Sound.MIDI.File.Save as Save

import qualified Data.Rhythm.MIDI as MIDI

import Debug.Trace

import System.Environment (getArgs)

autoplayEvent :: PG.DiffEvent -> [MPA.GtrMessage]
autoplayEvent (PG.Note str frt typ) = case typ of
  PG.ArpeggioForm -> []
  _ -> [MPA.Fret str' frt', MPA.Strum str' vel] where
    str' = toEnum $ fromEnum str
    frt' = MPA.GtrFret frt
    vel = V.toVelocity 96
autoplayEvent _ = []

autoplay :: (NN.C t) => RTB.T t PG.DiffEvent -> RTB.T t MPA.GtrMessage
autoplay = RTB.cons NN.zero MPA.KeepAlive . RTB.flatten . fmap autoplayEvent

getDiffEvents :: (NN.C t) => Difficulty -> RTB.T t (PG.T t) -> RTB.T t PG.DiffEvent
getDiffEvents d = RTB.mapMaybe $ \x -> case x of
  Length _ (PG.DiffEvent d' evt) | d == d' -> Just evt
  _ -> Nothing

squier :: RTB.T t MPA.GtrMessage -> RTB.T t MPA.GtrEvent
squier = fmap $ MPA.GtrEvent MPA.Squier

toMIDI :: RTB.T t MPA.GtrEvent -> RTB.T t E.T
toMIDI = fmap $ E.SystemExclusive . SysEx.Regular . (++ [0xF7]) . MPA.fromGtrEvent

-------------

pgToMIDI :: (NN.C t) => Difficulty -> RTB.T t (PG.T t) -> RTB.T t E.T
pgToMIDI d = toMIDI . squier . autoplay . getDiffEvents d

data PGTrack = PG | PB | PG22 | PB22
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

getPG :: PGTrack -> MIDI.File Bool -> RTB.T Beats (PG.T Beats)
getPG pgt mid = case MIDI.getTrack name mid of
  Nothing -> RTB.empty
  Just trk -> case interpretRTB PG.interpret $ toUnified trk of
    (pg, _, _) -> pg
  where name = case pgt of
          PG -> "PART REAL_GUITAR"
          PB -> "PART REAL_BASS"
          PG22 -> "PART REAL_GUITAR_22"
          PB22 -> "PART REAL_BASS_22"

run :: PGTrack -> Difficulty -> FilePath -> FilePath -> IO ()
run pgt d fin fout = Load.fromFile fin >>= \f -> case MIDI.standardFile f of
  Nothing -> putStrLn "Not a type-1 ticks-based MIDI file"
  Just m -> let
    auto = fmap MIDI.standardEvent $ pgToMIDI d $ getPG pgt m
    m' = m { MIDI.tracks = [(Just "Autoplay", auto)] }
    in case MIDI.rawFile m' of
      Left _ -> putStrLn "time signature error"
      Right f -> Save.toFile fout f

main = getArgs >>= \argv -> case argv of
  [pgt, d, fin, fout] -> run (read pgt) (read d) fin fout
  _ -> mapM_ putStrLn
    [ "Usage: pgautoplay track diff file-in file-out"
    , "track is one of: PG22 PB22"
    , "diff is one of: Easy Medium Hard Expert" ]
