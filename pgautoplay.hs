{- |
Generates the appropriate MIDI Pro Adapter inputs to (hopefully) FC a Pro Guitar
chart. Requires the rhythm, rhythm-midi, rhythm-rockband, and midiproadapter
packages.
-}
module Main where

import Data.Rhythm.Event
import Data.Rhythm.Parser
import qualified Data.Rhythm.MIDI as MIDI
import Data.Rhythm.RockBand.Common
import qualified Data.Rhythm.RockBand.Lex.ProGuitar as PG

import qualified Data.RockBand.MIDIProAdapter as MPA

import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Rhythm.EventList as RTB
import qualified Numeric.NonNegative.Class as NN

import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.SystemExclusive as SysEx
import qualified Sound.MIDI.File.Load as Load
import qualified Sound.MIDI.File.Save as Save

import System.Environment (getArgs)
import Data.Char (toUpper)
import Control.Applicative
import Data.Maybe
import Control.Monad.Trans.State
import Data.List (nubBy)
import Data.Function (on)

import qualified Data.Map as Map

-- | Removes redundant fret change events.
cleanup :: (NN.C t) => RTB.T t MPA.GtrMessage -> RTB.T t MPA.GtrMessage
cleanup rtb = evalState (RTB.filterA f rtb) Map.empty where
  f :: MPA.GtrMessage -> State (Map.Map MPA.GtrString MPA.GtrFret) Bool
  f (MPA.Fret str frt) = do
    redundant <- gets $ \mp -> Map.lookup str mp == Just frt
    if redundant then return False
      else modify (Map.insert str frt) >> return True
  f _ = return True

playEvent :: [PG.DiffEvent] -> [MPA.GtrMessage]
playEvent evts = fretMsgs ++ strumMsgs where

  strumMsg s = MPA.Strum (toEnum $ fromEnum s) $ V.toVelocity 96
  strummed = forMaybe evts $ \evt -> case evt of
    PG.Note s _ t | notElem t [PG.ArpeggioForm, PG.Tapped] -> Just s
    _ -> Nothing
  strumMsgs = map strumMsg strummed
  
  fretMsg (s, f) = MPA.Fret (toEnum $ fromEnum s) $ MPA.GtrFret f
  fretted = forMaybe evts $ \evt -> case evt of
    PG.Note s f t | t /= PG.ArpeggioForm -> Just (s, fromIntegral f)
    _ -> Nothing
  fretZero = [ (s, 0) | s <- [minBound .. maxBound] ]
  fretMsgs = map fretMsg $ nubBy ((==) `on` fst) $ fretted ++ fretZero
  
  forMaybe = flip mapMaybe

play :: (NN.C t) => RTB.T t PG.DiffEvent -> RTB.T t MPA.GtrMessage
play = cleanup . RTB.mapCoincident playEvent

getDiffEvents :: (NN.C t) =>
  Difficulty -> RTB.T t (PG.T t) -> RTB.T t PG.DiffEvent
getDiffEvents d = RTB.mapMaybe $ \x -> case x of
  Length _ (PG.DiffEvent d' evt) | d == d' -> Just evt
  _ -> Nothing

toMIDI :: RTB.T t MPA.GtrEvent -> RTB.T t E.T
toMIDI =
  fmap $ E.SystemExclusive . SysEx.Regular . (++ [0xF7]) . MPA.fromGtrEvent

-------------

pgToMIDI :: (NN.C t) =>
  MPA.GtrController -> Difficulty -> RTB.T t (PG.T t) -> RTB.T t E.T
pgToMIDI c d = toMIDI . fmap (MPA.GtrEvent c) . play . getDiffEvents d

data Instrument = Guitar | Bass
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

getPG :: (NN.C t, Num t, Show t) =>
  MPA.GtrController -> Instrument -> MIDI.File t Bool -> RTB.T t (PG.T t)
getPG c i mid = go $ fromMaybe (error "MIDI track not found") $ case c of
  MPA.Squier  -> MIDI.getTrack name22 mid <|> MIDI.getTrack name17 mid
  MPA.Mustang -> MIDI.getTrack name17 mid
  where go t = RTB.catMaybes $ fst $ parseEvents PG.parse $ unifyEvents t
        name17 = "PART REAL_" ++ map toUpper (show i)
        name22 = name17 ++ "_22"

run ::
  MPA.GtrController -> Instrument -> Difficulty -> FilePath -> FilePath -> IO ()
run c i d fin fout = Load.fromFile fin >>= \f -> case MIDI.readFile f of
  Nothing -> putStrLn "Not a type-1 ticks-based MIDI file"
  Just m  -> let
    auto = fmap MIDI.readEvent $ pgToMIDI c d $ getPG c i m
    m' = m { MIDI.tracks = [(Just "Autoplay", auto)] }
    in Save.toFile fout $ MIDI.showFile m'

main :: IO ()
main = getArgs >>= \argv -> case argv of
  [c, i, d, fin, fout] -> run (read c) (read i) (read d) fin fout
  _ -> mapM_ putStrLn
    [ "Usage: pgautoplay cont inst diff file-in file-out"
    , "cont is one of: Squier Mustang"
    , "inst is one of: Guitar Bass"
    , "diff is one of: Easy Medium Hard Expert" ]
