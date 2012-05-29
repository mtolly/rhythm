-- | Handwritten parser for FeedBack .chart format
module Data.FeedBack.Parse where

import Data.FeedBack.Base
import Data.FeedBack.Lex
import Control.Monad

newtype Parser tok a =
  Parser { runParser :: [tok] -> Either String (a, [tok]) }

instance Functor (Parser tok) where
  fmap g (Parser p) = Parser $ \toks -> case p toks of
    Left str -> Left str
    Right (x, toks') -> Right (g x, toks')

instance Monad (Parser tok) where
  return x = Parser $ \toks -> Right (x, toks)
  p >>= f = Parser $ \toks ->
    runParser p toks >>= \(x, toks') -> runParser (f x) toks'

-- generate a parse error message
pError :: String -> Parser (AlexPosn, tok) a
pError str = Parser $ \toks -> case toks of
  [] -> Left $ "Parse error at EOF: " ++ str
  (AlexPn _ ln col, _) : _ ->
    Left $ "Parse error at " ++ show ln ++ ":" ++ show col ++ ": " ++ str

-- drop initial newlines
pSpace :: Parser (AlexPosn, Token) ()
pSpace = Parser $
  \toks -> Right ((), dropWhile (\(_, tok) -> tok == Newline) toks)

-- satisfy a predicate
pSatisfy :: (tok -> Bool) -> String -> Parser (AlexPosn, tok) tok
pSatisfy f str = Parser $ \toks -> case toks of
  (_, tok) : rest | f tok -> Right (tok, rest)
  _ -> runParser (pError ("expected " ++ str)) toks

-- match an exact token
pToken :: (Eq tok, Show tok) => tok -> Parser (AlexPosn, tok) tok
pToken tok = pSatisfy (== tok) (show tok)

-- match and extract an identifier
pIdent :: Parser (AlexPosn, Token) String
pIdent = Parser $ \toks -> case toks of
  (_, TValue (VIdent str)) : rest -> Right (str, rest)
  _ -> runParser (pError "expected identifier") toks

-- repeat a parser zero or more times, never fails
pMany :: Parser tok a -> Parser tok [a]
pMany p = Parser $ \toks -> case runParser p toks of
  Left _ -> Right ([], toks)
  Right (x, toks') -> runParser (fmap (x:) $ pMany p) toks'

-- repeat a parser one or more times, only fails if first time fails
pMany1 :: Parser tok a -> Parser tok [a]
pMany1 p = liftM2 (:) p (pMany p)

-- parse [ChunkName]{lines}
pChunk :: Parser (AlexPosn, Token) (String, RawChunk)
pChunk = do
  _ <- pSpace >> pToken LBracket
  name <- pSpace >> pIdent
  _ <- pSpace >> pToken RBracket
  _ <- pSpace >> pToken LBrace
  lns <- pMany pLine
  _ <- pSpace >> pToken RBrace
  return (name, lns)

-- parse a single data value (int, float, str, ident)
pValue :: Parser (AlexPosn, Token) Value
pValue = Parser $ \toks -> case toks of
  (_, TValue v) : rest -> Right (v, rest)
  _ -> runParser (pError "expected value") toks

-- parse a chunk line "val = val val val ..."
pLine :: Parser (AlexPosn, Token) (Value, [Value])
pLine = liftM2 (,) (pSpace >> pValue) (pToken Equals >> pMany1 pValue)
