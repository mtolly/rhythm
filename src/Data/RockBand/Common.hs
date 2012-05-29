-- | Types which are used across multiple Rock Band instruments.
module Data.RockBand.Common where

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

newtype GtrFret = GtrFret { fromGtrFret :: Int }
  deriving (Eq, Ord, Show, Read)
