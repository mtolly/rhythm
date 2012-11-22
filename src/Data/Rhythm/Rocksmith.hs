module Data.Rhythm.Rocksmith where

import Data.Rhythm.Time
import Data.Rhythm.Guitar
import Data.Rhythm.Event
import Data.Time
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Numeric.NonNegative.Wrapper as NN

data Song t a = Song
  { title :: String
  , arrangement :: String
  , part :: Int
  , offset :: Offset t
    -- usually -10 seconds for normal songs, can be slightly different
    -- some technique challenges have 0
  , songLength :: t
  , lastConversion :: UTCTime
  , phrases :: [Phrase]
  , phraseIterations :: RTB.T t Int -- Int is phraseId
  , linkedDiffs :: [LinkedDiff]
  , phraseProperties :: [PhraseProperty]
  , chordTemplates :: [ChordTemplate]
  , fretHandMuteTemplates :: () -- TODO none observed on disc
  , ebeats :: RTB.T t (Maybe NN.Int) -- Nothing is -1, can be Bool/Beat?
  , sections :: RTB.T t (String, Int) -- (name, number)
  , events :: RTB.T t String
  , levels :: [(Int, Level t a)] -- Int is difficulty
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
  { phraseDifficulty :: Maybe NN.Int -- Nothing is -1
  , empty :: Bool
  , levelJump :: Int -- always 0 on disc, might be Bool
  , phraseId :: Int
  , redundant :: Bool
  } deriving (Eq, Ord, Show)

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

type Level t a = RTB.T t (Event Length Point a)

data Length
  = NoteEvent Note
  | HandShape Int -- chordId
  deriving (Eq, Ord, Show)

instance Long Length

data Point
  = ChordEvent Chord
  | FretHandMute -- TODO none observed on disc
  | Anchor GtrFret
  deriving (Eq, Ord, Show)

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
  , tremolo :: Bool
  } deriving (Eq, Ord, Show)

data Chord = Chord
  { chordId :: Int
  , highDensity :: Bool
  , ignoreChord :: Bool
  , strum :: String
  } deriving (Eq, Ord, Show, Read)

type Vocals t a = RTB.T t (Event' Vocal a)

data Vocal = Vocal
  { note :: V.Pitch
  , lyric :: String
  } deriving (Eq, Ord, Show)
