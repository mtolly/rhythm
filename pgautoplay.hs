module Main where

-- pro guitar autoplayer
-- example app using rhythm and midiproadapter libraries

import Data.Rhythm.Event
import Data.Rhythm.Interpret
import Data.Rhythm.Time
import qualified Data.Rhythm.MIDI as MIDI
import Data.Rhythm.RockBand.Common
import qualified Data.Rhythm.RockBand.Lex.ProGuitar as PG

import qualified Data.RockBand.MIDIProAdapter as MPA

import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Numeric.NonNegative.Class as NN

import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.SystemExclusive as SysEx
import qualified Sound.MIDI.File.Load as Load
import qualified Sound.MIDI.File.Save as Save

import System.Environment (getArgs)
import Data.Char (toUpper)
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)

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

toMIDI :: RTB.T t MPA.GtrEvent -> RTB.T t E.T
toMIDI = fmap $ E.SystemExclusive . SysEx.Regular . (++ [0xF7]) . MPA.fromGtrEvent

-------------

pgToMIDI :: (NN.C t) => MPA.GtrController -> Difficulty -> RTB.T t (PG.T t) -> RTB.T t E.T
pgToMIDI c d = toMIDI . fmap (MPA.GtrEvent c) . autoplay . getDiffEvents d

data Instrument = Guitar | Bass
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

getPG :: MPA.GtrController -> Instrument -> MIDI.File Bool -> RTB.T Beats (PG.T Beats)
getPG c i mid = go $ fromMaybe (error "MIDI track not found") $ case c of
  MPA.Squier -> MIDI.getTrack name22 mid <|> MIDI.getTrack name17 mid
  MPA.Mustang -> MIDI.getTrack name17 mid
  where go t = case interpretRTB PG.interpret $ toUnified t of (pg, _, _) -> pg
        name17 = "PART REAL_" ++ map toUpper (show i)
        name22 = name17 ++ "_22"

run :: MPA.GtrController -> Instrument -> Difficulty -> FilePath -> FilePath -> IO ()
run c i d fin fout = Load.fromFile fin >>= \f -> case MIDI.standardFile f of
  Nothing -> putStrLn "Not a type-1 ticks-based MIDI file"
  Just m -> let
    auto = fmap MIDI.standardEvent $ pgToMIDI c d $ getPG c i m
    m' = m { MIDI.tracks = [(Just "Autoplay", auto)] }
    in case MIDI.rawFile m' of
      Left _ -> putStrLn "time signature error"
      Right f -> Save.toFile fout f

main = getArgs >>= \argv -> case argv of
  [c, i, d, fin, fout] -> run (read c) (read i) (read d) fin fout
  _ -> mapM_ putStrLn
    [ "Usage: pgautoplay cont inst diff file-in file-out"
    , "cont is one of: Squier Mustang"
    , "inst is one of: Guitar Bass"
    , "diff is one of: Easy Medium Hard Expert" ]
