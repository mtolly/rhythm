{
-- | Generated parser for FeedBack .chart format
module Data.Rhythm.FeedBack.Parse (parse, fromFile) where

import qualified Numeric.NonNegative.Wrapper as NN
import Data.Rhythm.FeedBack.Base
import Data.Rhythm.FeedBack.Lex
import Data.Rhythm.Types
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.EventList.Absolute.TimeBody as ATB
}

%name parse
%tokentype { (AlexPosn, Token) }
%error { parseError }

%token
  int { (_, TValue (Int $$)) }
  real { (_, TValue (Real $$)) }
  quoted { (_, TValue (Quoted $$)) }
  ident { (_, TValue (Ident $$)) }
  '{' { (_, LBrace) }
  '}' { (_, RBrace) }
  '[' { (_, LBracket) }
  ']' { (_, RBracket) }
  '=' { (_, Equals) }
  song { (_, Song) }
  synctrack { (_, SyncTrack) }
  b { (_, B) }
  a { (_, A) }
  ts { (_, TS) }
  e { (_, E) }
  n { (_, N) }
  s { (_, S) }

%%

File : SongChunk SyncChunk EventChunks { File $1 $2 $3 }

EventChunks : EventChunk EventChunks { $1 : $2 }
            | { [] }

SongChunk : '[' song ']' '{' SongLines '}' { $5 }
SyncChunk : '[' synctrack ']' '{' EventLines '}' { toEventChunk $5 }
EventChunk : '[' ident ']' '{' EventLines '}' { ($2, toEventChunk $5) }

SongLines : ident '=' Value SongLines { ($1, $3) : $4 }
          | { [] }

EventLines : EventLine EventLines { $1 : $2 }
           | { [] }

EventLine : int '=' Event { ($1, $3) }

Event : b int { Point $ BPM $ fromIntegral $2 / 1000 }
      | a int { Point $ Anchor $ fromIntegral $2 / 1000000 }
      | ts int { Point $ TSig $2 }
      | e quoted { Point $ EventGlobal $2 }
      | e ident { Point $ EventLocal $2 }
      | n int int { Duration (fromIntegral $3) (Note $2) }
      | s int int { Duration (fromIntegral $3) (Stream $2) }

Value : int { Int $1 }
      | real { Real $1 }
      | quoted { Quoted $1 }
      | ident { Ident $1 }

{

fromFile :: FilePath -> IO (File Ticks)
fromFile fp = fmap (parse . scan) $ readFile fp

-- | If instead of this error, "Internal Happy error" is sometimes printed, make
-- sure you are using Happy 1.18.7 or later.
parseError :: [(AlexPosn, Token)] -> a
parseError [] = error "Parse error at EOF"
parseError ((AlexPn _ ln col, tok) : _) = error $
  "Parse error at " ++ show ln ++ ":" ++ show col ++ ", token " ++ show tok

toEventChunk :: [(Ticks, Event Ticks)] -> EventChunk Ticks
toEventChunk = RTB.fromAbsoluteEventList . ATB.fromPairList

}
