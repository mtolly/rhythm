{- | Autoplay module for Drums and Pro Drums. Using this, you can take a MIDI
     containing a drums chart, and create a MIDI file that should be played back
     directly into the MIDI Pro Adapter. -}
module Data.RockBand.Input.Drums where

import Data.RockBand.Common
import Control.Monad.Trans.State
import qualified Data.RockBand.Lexer.Drums as M
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Numeric.NonNegative.Class as NNC
import qualified Data.RockBand.Lexer.MIDI as MIDI
import qualified Sound.MIDI.Message.Channel.Voice as V
import Data.MusicTime

-- | A pad on a Basic/Pro Drums kit.
data Pad
  = Kick
  | Red
  | YellowTom
  | BlueTom
  | GreenTom
  | YellowCymbal
  | BlueCymbal
  | GreenCymbal
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

{- | When extracting pad hits from the drum MIDI track, we need to keep track
     of whether notes are toms or cymbals, and whether discoflip is on. -}
data ProDrumsState = ProDrumsState
  { yellowToms :: Bool
  , blueToms :: Bool
  , greenToms :: Bool
  , discoBeat :: Bool }

{- | Take one event from the drums MIDI track, and possibly output a real
     pad to hit. -}
process :: Difficulty -> M.Event Bool -> State ProDrumsState (Maybe Pad)
-- First, if there's a toms marker, update the state.
process _ (Duration (M.Toms M.Yellow) b) =
  modify (\s -> s { yellowToms = b }) >> return Nothing
process _ (Duration (M.Toms M.Blue) b) =
  modify (\s -> s { blueToms = b }) >> return Nothing
process _ (Duration (M.Toms M.Green) b) =
  modify (\s -> s { greenToms = b }) >> return Nothing
-- Else, if there's a discobeat event, update the state.
process d (Point (M.DiffEvent d' (M.Mix _ dsc))) | d == d' = case dsc of
  M.Disco -> modify (\s -> s { discoBeat = True }) >> return Nothing
  _ -> modify (\s -> s { discoBeat = False }) >> return Nothing
{- Else, if it's a note, output a real pad to hit. Consult the state to find
   out whether to use toms or cymbals, and whether to flip red/yellow. -}
process d (Point (M.DiffEvent d' (M.Note dr))) | d == d' = case dr of
  M.Kick -> return $ Just Kick
  M.Red -> gets discoBeat >>= \b ->
    return $ Just $ if b then YellowCymbal else Red
  M.Yellow -> gets yellowToms >>= \t -> gets discoBeat >>= \b ->
    return $ Just $ case (t, b) of
      (_, True) -> Red
      (True, _) -> YellowTom
      _         -> YellowCymbal
  M.Blue -> gets blueToms >>= \t ->
    return $ Just $ if t then BlueTom else BlueCymbal
  M.Green -> gets greenToms >>= \t ->
    return $ Just $ if t then GreenTom else GreenCymbal
process _ _ = return Nothing

{- | Gets the real notes for a Pro Drums track. -}
getProNotes :: (NNC.C t) => Difficulty -> RTB.T t (M.Event Bool) -> RTB.T t Pad
{- We use RTB.normalize, because if there's a 'M.Toms' or 'M.Mix' event that
   coincides with a note, the toms or mix event has to be processed first. The
   constructor order for 'M.Event's, together with the derived 'Ord' instance,
   ensures that this happens. -}
{- Note that there's a bug in Rock Band 3 where discobeat events aren't
   processed at the correct time; they're slightly late, which leads to wrong
   notes in songs like \"Flirtin' With Disaster\". At some point I'll come up
   with a workaround for this. Or, y'know, Harmonix could fix their bug. -}
getProNotes d
  = RTB.catMaybes
  . (`evalState` ProDrumsState False False False False)
  . RTB.mapBodyM (process d)
  . RTB.normalize

{- | Gets the real notes for a Basic Drums track. -}
getBasicNotes :: (NNC.C t) => Difficulty -> RTB.T t (M.Event Bool) -> RTB.T t Pad
-- Just use the Pro function, but remove all toms and mix events.
getBasicNotes d = getProNotes d . RTB.filter f where
  f (Duration (M.Toms _) _) = False
  f (Point (M.DiffEvent _ (M.Mix _ _))) = False
  f _ = True

{- | Converts a Pad to its canonical MIDI note number, as recognized by the
     MIDI Pro Adapter. -}
padToPitch :: Pad -> V.Pitch
padToPitch p = V.toPitch $ case p of
  Kick -> 33
  Red -> 38
  YellowTom -> 48
  BlueTom -> 45
  GreenTom -> 41
  YellowCymbal -> 22
  BlueCymbal -> 51
  GreenCymbal -> 49

-- | Converts each pad hit to a 128th note (1/32 of a beat) MIDI drum note.
toMIDI :: BeatTrack Pad -> BeatTrack' MIDI.Event
toMIDI = RTB.mapBody (MIDI.blip . padToPitch)
