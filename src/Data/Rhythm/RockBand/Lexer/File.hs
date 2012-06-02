-- | The datatype that contains an entire Rock Band .mid file.
module Data.Rhythm.RockBand.Lexer.File where

import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Rhythm.RockBand.Lexer.Drums as Drums
import qualified Data.Rhythm.RockBand.Lexer.Basic as Basic
import qualified Data.Rhythm.RockBand.Lexer.ProKeys as ProKeys
import qualified Data.Rhythm.RockBand.Lexer.ProGuitar as ProGuitar
import qualified Data.Rhythm.RockBand.Lexer.Vocal as Vocal
import qualified Data.Rhythm.RockBand.Lexer.Events as Events
import qualified Data.Rhythm.RockBand.Lexer.Beat as Beat

data RockBand t a = RockBand
  { partDrums        :: Maybe (RTB.T t (Drums.Event a))
  , partBass         :: Maybe (RTB.T t (Basic.Event a))
  , partGuitar       :: Maybe (RTB.T t (Basic.Event a))
  , partKeys         :: Maybe (RTB.T t (Basic.Event a))
  , partVocal        :: Maybe (RTB.T t (Vocal.Event a))
  , partRealBass     :: Maybe (RTB.T t (ProGuitar.Event a))
  , partRealBass22   :: Maybe (RTB.T t (ProGuitar.Event a))
  , partRealGuitar   :: Maybe (RTB.T t (ProGuitar.Event a))
  , partRealGuitar22 :: Maybe (RTB.T t (ProGuitar.Event a))
  , partRealKeysE    :: Maybe (RTB.T t (ProKeys.Event a))
  , partRealKeysM    :: Maybe (RTB.T t (ProKeys.Event a))
  , partRealKeysH    :: Maybe (RTB.T t (ProKeys.Event a))
  , partRealKeysX    :: Maybe (RTB.T t (ProKeys.Event a))
  , animKeysLH       :: Maybe (RTB.T t (ProKeys.Event a))
  , animKeysRH       :: Maybe (RTB.T t (ProKeys.Event a))
  , harm1            :: Maybe (RTB.T t (Vocal.Event a))
  , harm2            :: Maybe (RTB.T t (Vocal.Event a))
  , harm3            :: Maybe (RTB.T t (Vocal.Event a))
  , events           :: Maybe (RTB.T t Events.Event)
  , beat             :: Maybe (RTB.T t Beat.Event)
  -- , venue :: Maybe (RTB.T t Venue.Event)
  } deriving (Eq, Ord, Show)
