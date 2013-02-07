module Data.Rhythm.RockBand.Lex.VenueRB3 where

import Data.Rhythm.RockBand.Common
import qualified Data.Rhythm.MIDI as MIDI
import Data.Rhythm.Event
import Data.Rhythm.Time
import Data.Rhythm.Interpret
import qualified Numeric.NonNegative.Class as NN
import Data.Char (toLower)
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as M

data Length
  = SingAlong Instrument
  | Spotlight Instrument

data CoopCamera
  = AllFar
  | All Distance
  | Front Distance
  | One Instrument Distance
  | VoxCloseup
  | HandCloseup Instrument
  | HeadCloseup Instrument
  | Two Instrument Instrument Distance

data Distance = Behind | Near

data Instrument = Drums | Vocals | Bass | Guitar | Keys

data Directed
  = All
  | AllCam
  | AllLT
  | AllYeah
  | BRE
  | BREJ
  | NP Instrument
  | Hit Instrument
  | DrumsLT
  | CamPR Instrument -- vox, gtr
  | CamPT Instrument -- vox, gtr
  | Cam Instrument -- keys, bass
  | StageDive
  | CrowdSurf
  | Close Instrument
  | DrumsPoint
  | CrowdInteract -- gtr, bass
  | DuoDrums
  | Duo Instrument Instrument
  | Crowd

data PostProcess
  = ProFilmA
  | ProFilmB
  | VideoA
  | Film16MM
  | ShittyTV
  | Bloom
  | FilmSepiaInk
  | FilmSilvertone
  | FilmBW
  | VideoBW
  | ContrastA
  | Photocopy
  | FilmBlueFilter
  | DesatBlue
  | VideoSecurity
  | Bright
  | Posterize
  | CleanTrails
  | VideoTrails
  | FlickerTrails
  | DesatPosterizeTrails
  | FilmContrast
  | FilmContrastBlue
  | FilmContrastGreen
  | FilmContrastRed
  | HorrorMovieSpecial
  | PhotoNegative
  | ProFilmMirrorA
  | ProFilmPsychedelicBlueRed
  | SpaceWoosh
