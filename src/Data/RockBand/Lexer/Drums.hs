{-# LANGUAGE PatternGuards, ViewPatterns #-}
-- | The contents of the \"PART DRUMS\" track.
module Data.RockBand.Lexer.Drums where

import Data.RockBand.Common
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Data.RockBand.Lexer.MIDI as MIDI
import Data.MusicTime
import Data.List (stripPrefix)

{- | All the events in the \"PART DRUMS\" track, which contain all the drum
     notes, as well as the drummer's animations. -}
type Event = TimeEvent Duration Point

data Duration
  -- | Change 'Yellow', 'Blue', and 'Green' cymbals to toms.
  = Toms Drum
  -- | An overdrive phrase which fills 1/4 of the player's energy bar.
  | Overdrive
  -- | Drum fill lanes for Overdrive activation, as well as Big Rock Endings.
  | Activation
  -- | A drum solo section. You can't have activation fills during a solo.
  | Solo
  -- | A \"standard\" fill lane, for one tom or cymbal pad (not kick).
  | SingleRoll
  -- | A \"special\" fill lane, for two pads.
  | DoubleRoll
  -- | Used pre-RB3 for Tug of War mode.
  | Player1
  -- | Used pre-RB3 for Tug of War mode.
  | Player2
  -- | An animation event that causes the drummer's hihat to be up (open).
  | HihatOpen
  deriving (Eq, Ord, Show, Read)

data Point
  = Animation Animation -- ^ The drummer's animation events.
  | Mood Mood -- ^ The drummer's playing mood.
  | DiffEvent Difficulty DiffEvent -- ^ Events for a specific difficulty.
  deriving (Eq, Ord, Show, Read)

data DiffEvent
  -- | Changes the drum audio & discobeat settings.
  = Mix Audio Disco
  {- | A drum note, for a certain difficulty. Use 'Toms' events to select
       between tom and cymbal notes. -}
  | Note Drum
  deriving (Eq, Ord, Show, Read)

{- | The five note types in Basic Drums. To select between toms and cymbals
     for Pro Drums, use 'Toms' events. -}
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
  -- | All pads are normal.
  = NoDisco
  -- | Yellow pad is snare, red pad is hihat. When playing Pro Drums, these are
  -- then switched back, so red is snare and yellow cymbal is hihat.
  | Disco
  -- | New in RB3. Used for snare beats where accented hits are 'Yellow'.
  | DiscoNoFlip
  -- | Pre-RB3. Used for 'Easy' sections with no \"kit\" notes, only
  -- 'Red' and 'Kick'.
  | EasyMix
  -- | Pre-RB3. Used for 'Easy' sections with no 'Kick' notes, only 'Red' and
  -- \"kit\".
  | EasyNoKick
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Animation events for the drummer. Most are self-explanatory.
data Animation
  -- | The highest-pitch tom.
  = Tom1 Hand
  -- | The middle tom.
  | Tom2 Hand
  -- | The lowest-pitch tom.
  | FloorTom Hand
  -- | HihatOpen is a duration event, so it will come before Hihat.
  | Hihat Hand
  | Snare Hit Hand
  | Ride Hand
  -- | The left crash, closer to the hihat.
  | Crash1 Hit Hand
  -- | The right crash, closer to the ride.
  | Crash2 Hit Hand
  | KickRF
  | Crash1RHChokeLH
  | Crash2RHChokeLH
  | PercussionRH
  -- | When 'True', slow 'Ride' hits will have a special animation style.
  | RideSide Bool
  deriving (Eq, Ord, Show, Read)

-- | Used in 'Animation' events to show accented or ghost notes.
data Hit = SoftHit | HardHit
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
-- | Used in 'Animation' events to control which hand hits a drum.
data Hand = LH | RH
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

readEvent :: MIDI.Event dur -> Maybe [Event dur]
readEvent (Duration (MIDI.Note _ p _) len) = case V.fromPitch p of

  -- Animation
  24 -> Just [Point $ Animation KickRF]
  25 -> Just [Duration HihatOpen len]
  26 -> Just [Point $ Animation $ Snare HardHit LH]
  27 -> Just [Point $ Animation $ Snare HardHit RH]
  28 -> Just [Point $ Animation $ Snare SoftHit LH]
  29 -> Just [Point $ Animation $ Snare SoftHit RH]
  30 -> Just [Point $ Animation $ Hihat LH]
  31 -> Just [Point $ Animation $ Hihat RH]
  32 -> Just [Point $ Animation PercussionRH]
  -- 33 unused
  34 -> Just [Point $ Animation $ Crash1 HardHit LH]
  35 -> Just [Point $ Animation $ Crash1 SoftHit LH]
  36 -> Just [Point $ Animation $ Crash1 HardHit RH]
  37 -> Just [Point $ Animation $ Crash1 SoftHit RH]
  38 -> Just [Point $ Animation $ Crash2 HardHit RH]
  39 -> Just [Point $ Animation $ Crash2 SoftHit RH]
  40 -> Just [Point $ Animation Crash1RHChokeLH]
  41 -> Just [Point $ Animation Crash2RHChokeLH]
  42 -> Just [Point $ Animation $ Ride RH]
  43 -> Just [Point $ Animation $ Ride LH]
  44 -> Just [Point $ Animation $ Crash2 HardHit LH]
  45 -> Just [Point $ Animation $ Crash2 SoftHit LH]
  46 -> Just [Point $ Animation $ Tom1 LH]
  47 -> Just [Point $ Animation $ Tom1 RH]
  48 -> Just [Point $ Animation $ Tom2 LH]
  49 -> Just [Point $ Animation $ Tom2 RH]
  50 -> Just [Point $ Animation $ FloorTom LH]
  51 -> Just [Point $ Animation $ FloorTom RH]

  -- Notes
  i | let (oct, k) = quotRem i 12
    , 5 <= oct && oct <= 8
    , 0 <= k && k <= 4
    -> Just [Point $ DiffEvent (toEnum $ oct - 5) $ Note $ toEnum k]

  -- Drum solo
  103 -> Just [Duration Solo len]
  
  -- Tug of War phrases
  105 -> Just [Duration Player1 len]
  106 -> Just [Duration Player2 len]

  -- Toms markers
  110 -> Just [Duration (Toms Yellow) len]
  111 -> Just [Duration (Toms Blue) len]
  112 -> Just [Duration (Toms Green) len]
  
  -- Overdrive phrases
  116 -> Just [Duration Overdrive len]
  
  -- Activation fills / BRE
  120 -> Just [Duration Activation len]
  121 -> Just []
  122 -> Just []
  123 -> Just []
  124 -> Just []
  
  -- Rolls
  126 -> Just [Duration SingleRoll len]
  127 -> Just [Duration DoubleRoll len]
  
  _ -> Nothing

readEvent (Point (MIDI.TextEvent str)) = case str of
  (readMix -> Just p) -> Just [Point p]
  (readMood -> Just m) -> Just [Point $ Mood m]
  "[ride_side_true]" -> Just [Point $ Animation $ RideSide True]
  "[ride_side_false]" -> Just [Point $ Animation $ RideSide False]
  _ -> Nothing
readEvent _ = Nothing

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
  -- Hell yeah pattern guards

showEvent :: Event Beats -> [MIDI.Event Beats]
showEvent (Point p) = case p of
  Animation anim -> [showAnimation anim]
  Mood m -> [Point $ MIDI.TextEvent $ showMood m]
  DiffEvent diff ev -> case ev of
    Mix aud dsc -> [Point $ MIDI.TextEvent $ showMix diff aud dsc]
    Note drm ->
      [MIDI.blip $ V.toPitch $ (fromEnum diff + 5) * 12 + fromEnum drm]
showEvent (Duration d len) = case d of
  HihatOpen -> [dlen 25]
  Toms drm -> [dlen $ 108 + fromEnum drm]
  Solo -> [dlen 103]
  Player1 -> [dlen 105]
  Player2 -> [dlen 106]
  Overdrive -> [dlen 116]
  Activation -> map dlen [120..124]
  SingleRoll -> [dlen 126]
  DoubleRoll -> [dlen 127]
  where dlen i = Duration (MIDI.standardNote $ V.toPitch i) len

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

showAnimation :: Animation -> MIDI.Event Beats
showAnimation anim = case anim of
  KickRF -> blip 24
  -- HihatOpen (25) is Duration
  Snare HardHit LH -> blip 26
  Snare HardHit RH -> blip 27
  Snare SoftHit LH -> blip 28
  Snare SoftHit RH -> blip 29
  Hihat LH -> blip 30
  Hihat RH -> blip 31
  PercussionRH -> blip 32
  -- 33 unused
  Crash1 HardHit LH -> blip 34
  Crash1 SoftHit LH -> blip 35
  Crash1 HardHit RH -> blip 36
  Crash1 SoftHit RH -> blip 37
  Crash2 HardHit RH -> blip 38
  Crash2 SoftHit RH -> blip 39
  Crash1RHChokeLH -> blip 40
  Crash2RHChokeLH -> blip 41
  Ride RH -> blip 42
  Ride LH -> blip 43
  Crash2 HardHit LH -> blip 44
  Crash2 SoftHit LH -> blip 45
  Tom1 LH -> blip 46
  Tom1 RH -> blip 47
  Tom2 LH -> blip 48
  Tom2 RH -> blip 49
  FloorTom LH -> blip 50
  FloorTom RH -> blip 51
  RideSide True -> Point $ MIDI.TextEvent "[ride_side_true]"
  RideSide False -> Point $ MIDI.TextEvent "[ride_side_false]"
  where blip = MIDI.blip . V.toPitch
