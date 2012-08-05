-- | Types which are used across multiple Rock Band instruments.
module Data.Rhythm.RockBand.Common where

import Control.Monad (guard)
import Data.Char (isLetter)
import Control.Applicative ((<|>))
import Data.List (stripPrefix)

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

showTrainer :: Trainer -> String -> String
showTrainer (TrainerBegin n) inst =
  "[begin_" ++ inst ++ " song_trainer_" ++ inst ++ "_" ++ show n ++ "]"
showTrainer (TrainerEnd n) inst =
  "[end_" ++ inst ++ " song_trainer_" ++ inst ++ "_" ++ show n ++ "]"
showTrainer (TrainerNorm n) inst =
  "[" ++ inst ++ "_norm song_trainer_" ++ inst ++ "_" ++ show n ++ "]"

