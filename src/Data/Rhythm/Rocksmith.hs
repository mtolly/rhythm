module Data.Rhythm.Rocksmith where

import Data.Rhythm.Time
import Data.Rhythm.Guitar
import Data.Rhythm.Event
import Data.Time
import qualified Data.EventList.Relative.TimeBody as RTB

data Song = Song
  { title :: String
  , arrangement :: String
  , part :: Int
  , offset :: Rational -- seconds, can be negative
  , songLength :: Seconds
  , lastConversion :: UTCTime
  , phrases :: [Phrase]
  , phraseIterations :: RTB.T Seconds Int -- Int is phraseId
  , linkedDiffs :: [LinkedDiff]
  , phraseProperties :: [PhraseProperty]
  , chordTemplates :: [ChordTemplate]
  , fretHandMuteTemplates :: () -- TODO
  , ebeats :: RTB.T Seconds (Maybe Int) -- Nothing is -1. Could be Bool or Beat?
  , sections :: RTB.T Seconds (String, Int) -- (name, number)
  , events :: RTB.T Seconds String
  , levels :: [Level]
  } deriving (Eq, Ord, Show)

data Phrase = Phrase
  { disparity :: Bool
  , ignorePhrase :: Bool
  , maxDifficulty :: Int
  , name :: String
  , solo :: Bool
  } deriving (Eq, Ord, Show, Read)

data LinkedDiff = LinkedDiff
  { childId :: Int
  , parentId :: Int
  } deriving (Eq, Ord, Show, Read)

data PhraseProperty = PhraseProperty
  { phraseDifficulty :: Int -- can be -1
  , empty :: Bool
  , levelJump :: Int -- Int or Bool?
  , phraseId :: Int
  , redundant :: Bool
  } deriving (Eq, Ord, Show, Read)

data ChordTemplate = ChordTemplate
  { chordName :: String
  -- For all of these, Nothing means -1 in the XML
  , finger0 :: Maybe GtrFret
  , finger1 :: Maybe GtrFret
  , finger2 :: Maybe GtrFret
  , finger3 :: Maybe GtrFret
  , finger4 :: Maybe GtrFret
  , fret0 :: Maybe GtrFret
  , fret1 :: Maybe GtrFret
  , fret2 :: Maybe GtrFret
  , fret3 :: Maybe GtrFret
  , fret4 :: Maybe GtrFret
  , fret5 :: Maybe GtrFret
  } deriving (Eq, Ord, Show)

data Level = Level
  { levelDifficulty :: Int
  , notes :: RTB.T Seconds Note
  , chords :: RTB.T Seconds Chord
  , fretHandMutes :: () -- TODO
  , anchors :: RTB.T Seconds GtrFret
  , handShapes :: RTB.T Seconds (Event' Int Seconds) -- Int is chordId
  } deriving (Eq, Ord, Show)

data Note = Note
  { bend :: GtrFret -- the number of half-steps to bend up, 0 for no bend
  , fret :: GtrFret
  , hammerOn :: Bool
  , harmonic :: Bool
  , hopo :: Bool
  , ignoreNote :: Bool
  , palmMute :: Bool
  , pullOff :: Bool
  , slideTo :: Maybe GtrFret
  , string :: SixString
  , sustain :: Seconds
  , tremolo :: Bool
  } deriving (Eq, Ord, Show)

data Chord = Chord
  { chordId :: Int
  , highDensity :: Bool
  , ignoreChord :: Bool
  , strum :: String
  } deriving (Eq, Ord, Show, Read)
