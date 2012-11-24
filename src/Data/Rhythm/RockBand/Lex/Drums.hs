{-# LANGUAGE PatternGuards, ViewPatterns #-}
-- | The contents of the \"PART DRUMS\" track.
module Data.Rhythm.RockBand.Lex.Drums where

import Data.Rhythm.RockBand.Common
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as M
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Data.Rhythm.MIDI as MIDI
import Data.Rhythm.Time
import Data.Rhythm.Event
import Data.Rhythm.Parser
import Data.List (stripPrefix)
import qualified Numeric.NonNegative.Class as NN

data Length
  = Toms Drum -- ^ Change 'Yellow', 'Blue', and 'Green' cymbal notes to toms.
  | Overdrive -- ^ The phrase which fills the player's energy bar.
  | Activation -- ^ Fill lanes for Overdrive activation and Big Rock Endings.
  | Solo -- ^ A drum solo section.
  | SingleRoll -- ^ \"Standard\" drum roll lane on one pad.
  | DoubleRoll -- ^ \"Special\" drum roll lane on two pads.
  | Player1 -- ^ Used pre-RB3 for Tug of War mode.
  | Player2 -- ^ Used pre-RB3 for Tug of War mode.
  | HihatOpen -- ^ Animation event that raises (opens) the hihat pedal.
  deriving (Eq, Ord, Show, Read)

data Point
  = Animation Animation -- ^ The drummer's animation events.
  | Mood Mood -- ^ The drummer's playing mood.
  | DiffEvent Difficulty DiffEvent -- ^ Events for a specific difficulty.
  deriving (Eq, Ord, Show, Read)

instance Long Length
type T = Event Length Point

data DiffEvent
  = Mix Audio Disco -- ^ Set the drum audio & pad layout.
  | Note Drum -- ^ A drum track gem.
  deriving (Eq, Ord, Show, Read)

-- | The five note types in Basic Drums. To select between toms and cymbals for
-- Pro Drums, use 'Toms' events.
data Drum
  = Kick
  | Red
  | Yellow
  | Blue
  | Green
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Controls the audio files used for the drum track.
data Audio
  = D0 -- ^ One stereo mix for the whole kit.
  | D1 -- ^ Mono kick, mono snare, stereo kit.
  | D2 -- ^ Mono kick, stereo snare, stereo kit.
  | D3 -- ^ Stereo kick, stereo snare, stereo kit.
  | D4 -- ^ Mono kick, stereo kit (including snare).
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Special options that can affect drum audio and pad settings.
data Disco
  = NoDisco -- ^ All pads are normal.
  | Disco -- ^ Yellow snare, red hihat. \"Undone\" by Pro Drums.
  | DiscoNoFlip -- ^ New in RB3: snare beats where accented hits are 'Yellow'.
  | EasyMix -- ^ Pre-RB3. 'Easy' sections with only 'Red' and 'Kick' notes.
  | EasyNoKick -- ^ Pre-RB3. 'Easy' sections with no 'Kick' notes.
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Animation events for the drummer. Most are self-explanatory.
data Animation
  = Tom1 Hand -- ^ The high tom.
  | Tom2 Hand -- ^ The middle tom.
  | FloorTom Hand -- ^ The low tom.
  | Hihat Hand
  | Snare Hit Hand
  | Ride Hand
  | Crash1 Hit Hand -- ^ The left crash, closer to the hihat.
  | Crash2 Hit Hand -- ^ The right crash, closer to the ride.
  | KickRF
  | Crash1RHChokeLH
  | Crash2RHChokeLH
  | PercussionRH
  | RideSide Bool -- ^ Causes slow 'Ride' hits to animate differently.
  deriving (Eq, Ord, Show, Read)

-- | Used in 'Animation' events to show accented or ghost notes.
data Hit = SoftHit | HardHit deriving (Eq, Ord, Show, Read, Enum, Bounded)
-- | Used in 'Animation' events to control which hand hits a drum.
data Hand = LH | RH deriving (Eq, Ord, Show, Read, Enum, Bounded)

readAnimation :: V.Pitch -> Maybe Animation
readAnimation p = case V.fromPitch p of
  24 -> Just KickRF
  -- 25 HihatOpen is a Length event
  26 -> Just $ Snare HardHit LH
  27 -> Just $ Snare HardHit RH
  28 -> Just $ Snare SoftHit LH
  29 -> Just $ Snare SoftHit RH
  30 -> Just $ Hihat LH
  31 -> Just $ Hihat RH
  32 -> Just PercussionRH
  -- 33 unused
  34 -> Just $ Crash1 HardHit LH
  35 -> Just $ Crash1 SoftHit LH
  36 -> Just $ Crash1 HardHit RH
  37 -> Just $ Crash1 SoftHit RH
  38 -> Just $ Crash2 HardHit RH
  39 -> Just $ Crash2 SoftHit RH
  40 -> Just Crash1RHChokeLH
  41 -> Just Crash2RHChokeLH
  42 -> Just $ Ride RH
  43 -> Just $ Ride LH
  44 -> Just $ Crash2 HardHit LH
  45 -> Just $ Crash2 SoftHit LH
  46 -> Just $ Tom1 LH
  47 -> Just $ Tom1 RH
  48 -> Just $ Tom2 LH
  49 -> Just $ Tom2 RH
  50 -> Just $ FloorTom LH
  51 -> Just $ FloorTom RH
  _  -> Nothing

parse :: (NN.C a) => Parser (MIDI.T a) (Maybe (T a))
parse = get >>= \x -> case x of
  Length len n@(MIDI.Note _ p _) -> case V.fromPitch p of
    25 -> single $ Length len HihatOpen
    -- Notes
    i | let (oct, k) = quotRem i 12
      , 5 <= oct && oct <= 8
      , 0 <= k && k <= 4
      -> single $ Point $ DiffEvent (toEnum $ oct - 5) $ Note $ toEnum k
    103 -> single $ Length len Solo
    105 -> single $ Length len Player1
    106 -> single $ Length len Player2
    110 -> single $ Length len $ Toms Yellow
    111 -> single $ Length len $ Toms Blue
    112 -> single $ Length len $ Toms Green
    116 -> single $ Length len Overdrive
    120 -> single $ Length len Activation
    121 -> return Nothing
    122 -> return Nothing
    123 -> return Nothing
    124 -> return Nothing
    126 -> single $ Length len SingleRoll
    127 -> single $ Length len DoubleRoll
    _ -> case readAnimation p of
      Just anim -> single $ Point $ Animation anim
      Nothing -> unrecognized n
  Point (E.MetaEvent txt@(M.TextEvent str)) -> case str of
    (readMix -> Just p) -> single $ Point p
    (readMood -> Just m) -> single $ Point $ Mood m
    "[ride_side_true]" -> single $ Point $ Animation $ RideSide True
    "[ride_side_false]" -> single $ Point $ Animation $ RideSide False
    _ -> unrecognized txt
  Point p -> unrecognized p
  where single = return . Just

-- | Tries to interpret a string as an audio mix event.
readMix :: String -> Maybe Point
readMix str
  | Just (x : xs) <- stripPrefix "[mix " str
  , elem x "0123"
  , let dif = toEnum $ read [x]
  , Just (y : ys) <- stripPrefix " drums" xs
  , elem y "01234"
  , let aud = toEnum $ read [y]
  = fmap (DiffEvent dif . Mix aud) $ case ys of
    "]" -> Just NoDisco
    "d]" -> Just Disco
    "dnoflip]" -> Just DiscoNoFlip
    "easy]" -> Just EasyMix
    "easynokick]" -> Just EasyNoKick
    _ -> Nothing
  | otherwise = Nothing
  -- Pattern guards: they're pretty cool

unparse :: T Beats -> [MIDI.T Beats]
unparse (Point p) = case p of
  Animation anim -> [showAnimation anim]
  Mood m -> [Point . E.MetaEvent . M.TextEvent $ showMood m]
  DiffEvent diff ev -> (:[]) $ case ev of
    Mix aud dsc -> Point . E.MetaEvent . M.TextEvent $ showMix diff aud dsc
    Note drm -> blip $ V.toPitch $ (fromEnum diff + 5) * 12 + fromEnum drm
unparse (Length len d) = map dlen $ case d of
  HihatOpen -> [25]
  Toms drm -> [108 + fromEnum drm]
  Solo -> [103]
  Player1 -> [105]
  Player2 -> [106]
  Overdrive -> [116]
  Activation -> [120..124]
  SingleRoll -> [126]
  DoubleRoll -> [127]
  where dlen = Length len . standardNote . V.toPitch

showAnimation :: Animation -> MIDI.T Beats
showAnimation anim = case anim of
  KickRF -> blip' 24
  -- HihatOpen (25) is not an Animation
  Snare HardHit LH -> blip' 26
  Snare HardHit RH -> blip' 27
  Snare SoftHit LH -> blip' 28
  Snare SoftHit RH -> blip' 29
  Hihat LH -> blip' 30
  Hihat RH -> blip' 31
  PercussionRH -> blip' 32
  -- 33 unused
  Crash1 HardHit LH -> blip' 34
  Crash1 SoftHit LH -> blip' 35
  Crash1 HardHit RH -> blip' 36
  Crash1 SoftHit RH -> blip' 37
  Crash2 HardHit RH -> blip' 38
  Crash2 SoftHit RH -> blip' 39
  Crash1RHChokeLH -> blip' 40
  Crash2RHChokeLH -> blip' 41
  Ride RH -> blip' 42
  Ride LH -> blip' 43
  Crash2 HardHit LH -> blip' 44
  Crash2 SoftHit LH -> blip' 45
  Tom1 LH -> blip' 46
  Tom1 RH -> blip' 47
  Tom2 LH -> blip' 48
  Tom2 RH -> blip' 49
  FloorTom LH -> blip' 50
  FloorTom RH -> blip' 51
  RideSide True -> Point . E.MetaEvent $ M.TextEvent "[ride_side_true]"
  RideSide False -> Point . E.MetaEvent $ M.TextEvent "[ride_side_false]"
  where blip' = blip . V.toPitch

showMix :: Difficulty -> Audio -> Disco -> String
showMix diff aud dsc = "[mix " ++ x ++ " drums" ++ y ++ z ++ "]" where
  x = show $ fromEnum diff
  y = show $ fromEnum aud
  z = case dsc of
    NoDisco -> ""
    Disco -> "d"
    DiscoNoFlip -> "dnoflip"
    EasyMix -> "easy"
    EasyNoKick -> "easynokick"
