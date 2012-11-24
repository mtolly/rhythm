module Data.Rhythm.RockBand.Lex.File where

import qualified Data.EventList.Relative.TimeBody as RTB

import Data.Rhythm.Time
import Data.Rhythm.Parser
import qualified Data.Rhythm.RockBand.Lex.Drums as Drums
import qualified Data.Rhythm.RockBand.Lex.Basic as Basic
import qualified Data.Rhythm.RockBand.Lex.Vocals as Vocals
import qualified Data.Rhythm.RockBand.Lex.ProGuitar as ProGuitar
import qualified Data.Rhythm.RockBand.Lex.ProKeys as ProKeys
import qualified Data.Rhythm.RockBand.Lex.Beat as Beat
import qualified Data.Rhythm.RockBand.Lex.Events as Events
import qualified Data.Rhythm.MIDI as MIDI

data File t a = File
  { partDrums :: Maybe (RTB.T t (Drums.T a))
  , partBass :: Maybe (RTB.T t (Basic.T a))
  , partGuitar :: Maybe (RTB.T t (Basic.T a))
  , partKeys :: Maybe (RTB.T t (Basic.T a))
  , partVocals :: Maybe (RTB.T t (Vocals.T a))
  , harm1 :: Maybe (RTB.T t (Vocals.T a))
  , harm2 :: Maybe (RTB.T t (Vocals.T a))
  , harm3 :: Maybe (RTB.T t (Vocals.T a))
  , partRealGuitar :: Maybe (RTB.T t (ProGuitar.T a))
  , partRealGuitar22 :: Maybe (RTB.T t (ProGuitar.T a))
  , partRealBass :: Maybe (RTB.T t (ProGuitar.T a))
  , partRealBass22 :: Maybe (RTB.T t (ProGuitar.T a))
  , partRealKeysX :: Maybe (RTB.T t (ProKeys.T a))
  , partRealKeysH :: Maybe (RTB.T t (ProKeys.T a))
  , partRealKeysM :: Maybe (RTB.T t (ProKeys.T a))
  , partRealKeysE :: Maybe (RTB.T t (ProKeys.T a))
  , partKeysAnimLH :: Maybe (RTB.T t (ProKeys.T a))
  , partKeysAnimRH :: Maybe (RTB.T t (ProKeys.T a))
  , beat :: Maybe (RTB.T t Beat.T)
  , events :: Maybe (RTB.T t Events.T)
  } deriving (Eq, Ord, Show)

extract :: (Eq a) => a -> [(a, b)] -> Maybe (b, [(a, b)])
extract _ [] = Nothing
extract k (p@(x, y) : xs) = if k == x
  then Just (y, xs)
  else fmap (\(v, rest) -> (v, p : rest)) $ extract k xs

getTrack :: String -> Parser (MIDI.File t a) (RTB.T t (MIDI.T a))
getTrack str = get >>= \f -> maybe (fail s) return $ MIDI.getTrack str f
  where s = "Couldn't find track: " ++ str

parse :: Parser (MIDI.File Beats Beats) (File Beats Beats)
parse = undefined
