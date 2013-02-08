module Main where

import qualified Sound.MIDI.File.Load as Load
import qualified Data.Rhythm.MIDI as MIDI
import qualified Data.Rhythm.RockBand.Lex.Beat as Beat
import Data.Rhythm.TimeSignature
import qualified Data.InfList as I
import qualified Sound.MIDI.File.Save as Save
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = getArgs >>= \argv -> case argv of
  [n, fin, fout] -> run (read n) fin fout
  _ -> hPutStrLn stderr "usage: beatgen <num of measures> <file-in> <file-out>"

run :: Int -> FilePath -> FilePath -> IO ()
run n fin fout = do
  mid <- Load.fromFile fin
  case MIDI.readFile mid of
    Nothing -> hPutStrLn stderr "Not a valid (type-1 ticks-based) MIDI file"
    Just m -> let
      btbt = MIDI.unifyFile $ MIDI.fromTickFile m
      sigtrk = MIDI.signatureTrack btbt
      sigs = I.take n $ measureSignatures sigtrk
      bttrk = fmap Beat.unparse $ Beat.beatTrack sigs
      btbt' = btbt { MIDI.tracks = MIDI.tracks btbt ++ [(Just "BEAT", bttrk)] }
      mout = MIDI.showFile $ MIDI.toTickFile $ MIDI.splitFile btbt'
      in Save.toFile fout mout
