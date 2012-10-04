module Data.Rhythm.FeedBack.Show (toFile, showFile, showValue) where

import Data.Rhythm.FeedBack
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.EventList.Absolute.TimeBody as ATB
import Data.Rhythm.Time
import Data.Rhythm.Event

toFile :: FilePath -> File Ticks -> IO ()
toFile fp db = writeFile fp $ showFile db ""

showFile :: File Ticks -> ShowS
showFile (File son syn chs)
  = showSongChunk son
  . showEventChunk "SyncTrack" syn
  . compose (map (uncurry showEventChunk) chs)

compose :: [a -> a] -> a -> a
compose = foldr (.) id

startChunk :: String -> ShowS
startChunk str onto = "[" ++ str ++ "]\n{\n" ++ onto

showLine :: Value -> [Value] -> ShowS
showLine lhs rhs onto = concat
  ["\t", showValue lhs, " = ", unwords (map showValue rhs), "\n", onto]

endChunk :: ShowS
endChunk onto = "}\n" ++ onto

showSongChunk :: SongData -> ShowS
showSongChunk chunk = startChunk "Song" . compose (map f chunk) . endChunk where
  f (str, val) = showLine (Ident str) [val]

showEventChunk :: String -> Chunk Ticks -> ShowS
showEventChunk name chunk = startChunk name . middle . endChunk where
  middle = compose $ map f $ ATB.toPairList $ RTB.toAbsoluteEventList 0 chunk
  f (tks, evt) = showLine (Int tks) $ case evt of
    Length len d -> case d of
      Note fret -> [Ident "N", Int fret, Int len]
      Stream strType -> [Ident "S", Int strType, Int len]
    Point p -> case p of
      BPM bpm -> [Ident "B", Int $ floor $ bpm * 1000]
      Anchor secs -> [Ident "A", Int $ floor $ secs * 1000000]
      TimeSig i -> [Ident "TS", Int i]
      EventGlobal str -> [Ident "E", Quoted str]
      EventLocal str -> [Ident "E", Ident str]

showValue :: Value -> String
showValue (Int i) = show i
showValue (Real r) = show (realToFrac r :: Double)
showValue (Quoted s) = show s
showValue (Ident s) = s
