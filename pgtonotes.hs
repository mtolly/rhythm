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

import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Numeric.NonNegative.Class as NN

import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Load as Load
import qualified Sound.MIDI.File.Save as Save

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Traversable
import Control.Monad.Trans.State

playString :: (NN.C t) =>
  SixString -> SixTuning V.Pitch -> RTB.T t (PG.T a) -> RTB.T t (MIDI.T a)
playString str tun = RTB.mapMaybe f where
  f :: PG.T a -> Maybe (MIDI.T a)
  f = undefined
