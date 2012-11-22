-- | Types which are used across multiple Rock Band instruments.
module Data.Rhythm.RockBand.Common where

import Control.Monad (guard)
import Data.Char (isLetter)
import Control.Applicative ((<|>))
import Data.List (stripPrefix)
import qualified Sound.MIDI.Message.Channel as C
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Data.Rhythm.MIDI as MIDI
import Data.Rhythm.Time
import Data.Rhythm.Event

data Difficulty
  = Easy
  | Medium
  | Hard
  | Expert
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Mood
  = Idle
  | IdleIntense
  | IdleRealtime
  | Intense
  | Play
  | PlaySolo
  | Mellow
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

readMood :: String -> Maybe Mood
readMood str = case str of
  "[idle]" -> Just Idle
  "[idle_intense]" -> Just IdleIntense
  "[idle_realtime]" -> Just IdleRealtime
  "[intense]" -> Just Intense
  "[play]" -> Just Play
  "[play_solo]" -> Just PlaySolo
  "[mellow]" -> Just Mellow
  _ -> Nothing

showMood :: Mood -> String
showMood m = case m of
  Idle -> "[idle]"
  IdleIntense -> "[idle_intense]"
  IdleRealtime -> "[idle_realtime]"
  Intense -> "[intense]"
  Play -> "[play]"
  PlaySolo -> "[play_solo]"
  Mellow -> "[mellow]"

-- | Used to define training sections for Pro Guitar, Pro Bass, and Pro Keys.
data Trainer
  = TrainerBegin Int
  | TrainerNorm Int
  | TrainerEnd Int
  deriving (Eq, Ord, Show, Read)

-- | The returned String is the instrument-specific part: @pg@, @pb@, @key@.
readTrainer :: String -> Maybe (Trainer, String)
readTrainer str = readBegin <|> readNorm <|> readEnd where
  readBegin = stripPrefix "[begin_" str >>= Just . span isLetter >>= \(x,xs) ->
    stripPrefix " song_trainer_" xs >>= Just . span isLetter >>= \(y,ys) ->
      guard (x == y) >> stripPrefix "_" ys >>= \zs -> case reads zs of
        [(n, "]")] -> Just (TrainerBegin n, x)
        _ -> Nothing
  readNorm = stripPrefix "[" str >>= Just . span isLetter >>= \(x,xs) ->
    stripPrefix "_norm song_trainer_" xs >>= Just . span isLetter >>= \(y,ys) ->
      guard (x == y) >> stripPrefix "_" ys >>= \zs -> case reads zs of
        [(n, "]")] -> Just (TrainerBegin n, x)
        _ -> Nothing
  readEnd = stripPrefix "[end_" str >>= Just . span isLetter >>= \(x,xs) ->
    stripPrefix " song_trainer_" xs >>= Just . span isLetter >>= \(y,ys) ->
      guard (x == y) >> stripPrefix "_" ys >>= \zs -> case reads zs of
        [(n, "]")] -> Just (TrainerBegin n, x)
        _ -> Nothing

-- | The String parameter is the instrument-specific part: @pg@, @pb@, @key@.
showTrainer :: Trainer -> String -> String
showTrainer tr inst = case tr of
  TrainerBegin n ->
    "[begin_" ++ inst ++ " song_trainer_" ++ inst ++ "_" ++ show n ++ "]"
  TrainerEnd n ->
    "[end_" ++ inst ++ " song_trainer_" ++ inst ++ "_" ++ show n ++ "]"
  TrainerNorm n ->
    "[" ++ inst ++ "_norm song_trainer_" ++ inst ++ "_" ++ show n ++ "]"

-- | Creates a note on channel 0, with a velocity of 96.
standardNote :: V.Pitch -> MIDI.Note
standardNote p = MIDI.Note (C.toChannel 0) p (V.toVelocity 96)

-- | Creates a MIDI note of the minimum possible duration allowed by Rock Band.
-- In a valid Rock Band MIDI, this is guaranteed to not overlap other notes.
blip :: V.Pitch -> MIDI.T Beats
blip p = Length (1 / 32) $ standardNote p

-- | @stripSuffix \"def\" \"abcdef\" ==> Just \"abc\"@
stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix xs ys = fmap reverse $ stripPrefix (reverse xs) (reverse ys)
