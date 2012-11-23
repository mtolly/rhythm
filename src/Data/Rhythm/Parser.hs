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
-- * An event list
-- * A MIDI file, or similar list of event lists
-- * An XML file
-- * A DTA/DTB file
-- Common theme: a tree-like type. Parse warnings need to be tagged with a
-- string denoting their context inside the tree, and contexts nest inside of
-- each other.

-- | This can represent an error (required input was not found) or a warning
-- (given input was not recognized).
data Message = Message
  { messageString :: String
  , messageContext :: [String]
  } deriving (Eq, Ord, Show, Read)

newtype Parser a b = Parser { runParser :: a -> ([Message], Either Message b) }
  deriving (Functor)

instance Applicative (Parser a) where
  pure x = Parser $ \_ -> ([], Right x)
  (<*>) = ap

instance Monad (Parser a) where
  return = pure
  p >>= f = Parser $ \x -> case runParser p x of
    (warns, Left e) -> (warns, Left e)
    (warns, Right y) -> case runParser (f y) x of
      (warns', result) -> (warns ++ warns', result)
  fail str = Parser $ \_ -> ([], Left $ Message str [])

instance Cat.Category Parser where
  id = returnA
  p . q = q >>= \x -> inside x $ context "..." p

instance Arrow Parser where
  arr f = Parser $ \x -> ([], Right $ f x)
  first p = Parser $ \(x, q) -> runParser (fmap (\y -> (y, q)) p) x

context :: String -> Parser a b -> Parser a b
context ctxt p = Parser $ \x -> case runParser p x of
  (warns, result) -> (map addContext warns, case result of
    Left e -> Left $ addContext e
    Right _ -> result)
  where addContext msg = msg { messageContext = ctxt : messageContext msg }

inside :: a -> Parser a b -> Parser c b
inside x p = Parser $ \_ -> runParser p x

warn :: String -> Parser a ()
warn str = Parser $ \_ -> ([Message str []], Right ())

error :: String -> Parser a b
error str = Parser $ \_ -> ([], Left $ Message str [])

parseEvents
  :: (Show t, NN.C t, Num t) => Parser a b -> Parser (RTB.T t a) (RTB.T t b)
parseEvents p = Parser $ \evts -> let
  go = (\(t, x) -> runParser (context (show t) p) x) <$> attachAbsoluteTime evts
  attachAbsoluteTime :: (Num t, NN.C t) => RTB.T t a -> RTB.T t (t, a)
  attachAbsoluteTime rtb = RTB.zipWithBody (,)
    (ATB.getTimes $ RTB.toAbsoluteEventList NN.zero rtb) rtb
  msgs = [msg | (warns, r) <- RTB.getBodies go, msg <- warns ++ leftToList r]
  result = RTB.mapMaybe (eitherToMaybe . snd) go
  eitherToMaybe = either (const Nothing) Just
  leftToList = either (:[]) (const [])
  in (msgs, Right result)
