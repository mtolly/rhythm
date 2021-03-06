{-# LANGUAGE DeriveFunctor #-}
module Data.Rhythm.Parser where

import Control.Applicative
import Control.Monad
import qualified Control.Category as Cat
import Control.Arrow
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Numeric.NonNegative.Class as NN

-- Things we might want to "parse" a datatype from:
--   An event list
--   A MIDI file, or similar list of event lists
--   An XML file
--   A DTA/DTB file
-- Common theme: a tree-like type. Parse warnings need to be tagged with a
-- string denoting their context inside the tree, and contexts nest inside of
-- each other.

-- | This can represent an error (required input was not found) or a warning
-- (given input was not completely recognized).
data Message = Message
  { messageString :: String
  , messageContext :: [String]
  } deriving (Eq, Ord, Show, Read)

addContext :: String -> Message -> Message
addContext str msg = msg { messageContext = str : messageContext msg }

newtype Parser a b = Parser { runParser :: a -> ([Message], Either Message b) }
  deriving (Functor)

instance Applicative (Parser a) where
  pure x = Parser $ const ([], Right x)
  (<*>) = ap

instance Monad (Parser a) where
  return = pure
  p >>= f = Parser $ \x -> case runParser p x of
    (warns, Left e) -> (warns, Left e)
    (warns, Right y) -> case runParser (f y) x of
      (warns', result) -> (warns ++ warns', result)
  fail str = Parser $ const ([], Left $ Message str [])

instance Cat.Category Parser where
  id = returnA
  p . q = q >>= \x -> inside x $ context "..." p

instance Arrow Parser where
  arr f = Parser $ \x -> ([], Right $ f x)
  first p = Parser $ \(x, q) -> runParser (fmap (\y -> (y, q)) p) x

instance Alternative (Parser a) where
  empty = fail "Parse failed"
  p <|> q = Parser $ \x -> case runParser p x of
    (msgs, Left _) -> case runParser q x of
      (msgs', res) -> (msgs ++ msgs', res)
    r@(_, Right _) -> r

instance MonadPlus (Parser a) where
  mzero = empty
  mplus = (<|>)

get :: Parser a a
get = returnA

context :: String -> Parser a b -> Parser a b
context ctxt p = Parser $ \x -> case runParser p x of
  (warns, result) -> (map (addContext ctxt) warns, case result of
    Left e -> Left $ addContext ctxt e
    Right _ -> result)

inside :: a -> Parser a b -> Parser c b
inside x p = Parser $ const $ runParser p x

warn :: String -> Parser a ()
warn str = Parser $ const ([Message str []], Right ())

parseEvents :: (Show t, NN.C t, Num t) =>
  Parser a b -> RTB.T t a -> (RTB.T t b, [Message])
parseEvents p evts = let
  results = fmap (runParser p) evts
  success = RTB.mapMaybe (\(_, e) -> eitherToMaybe e) results
  msgs = RTB.flatten $ fmap (\(warns, l) -> warns ++ leftToList l) results
  msgList = map (\(t, msg) -> addContext (show t) msg) $ ATB.toPairList $
    RTB.toAbsoluteEventList NN.zero msgs
  eitherToMaybe = either (const Nothing) Just
  leftToList = either (:[]) (const [])
  in (success, msgList)

parseEvents' :: (Show t, NN.C t, Num t) =>
  Parser a b -> Parser (RTB.T t a) (RTB.T t b)
parseEvents' p = Parser $ \x -> case parseEvents p x of
  (y, msgs) -> (msgs, Right y)

unrecognized :: (Show c) => c -> Parser a b
unrecognized x = fail $ "Unrecognized: " ++ show x
