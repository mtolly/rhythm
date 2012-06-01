-- | Parsec 3 parser for FeedBack .chart format
module Data.FeedBack.Parsec where

import Data.FeedBack.Base
import Text.Parsec
import Text.Parsec.String
import qualified Numeric.NonNegative.Wrapper as NN
import Control.Applicative hiding (many, (<|>), optional)
import Control.Monad
import Data.Ratio

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

conseq :: Monad m => [m [a]] -> m [a]
conseq ms = liftM concat $ sequence ms

-- | Parses the sign character (if one exists) at the beginning of a number,
-- returning either -1 or +1.
pSign :: Parser NN.Integer
pSign = (char '-' >> return (-1))
  <|> (optional (char '+') >> return 1)

-- | Parses a non-negative integer, a simple string of decimal digits.
pInt :: Parser NN.Integer
pInt = NN.fromNumberUnsafe . read <$> many1 digit

-- | Parses the decimal point and following sequence of numbers, returning that
-- part as a decimal number.
pDecimal :: Parser Rational
pDecimal = do
  _ <- char '.'
  dgts <- many1 digit
  return $ read dgts % read ('1' : map (const '0') dgts)

-- | Parses either a number in either int or float notation.
pNum :: Parser Value
pNum = do
  sign <- pSign
  int <- pInt
  let optDecimal = do
        dec <- pDecimal
        return $ Real $ fromIntegral sign * (fromIntegral int + dec)
      optInt = return $ Int $ sign * int
      in optDecimal <|> optInt

pQuoted :: Parser String
pQuoted = read <$> conseq [string "\"", concat <$> many quotedChar, string "\""]
  where quotedChar, normalChar, escapedChar :: Parser String
        quotedChar = normalChar <|> escapedChar
        normalChar = (:[]) <$> noneOf "\"\\"
        escapedChar = liftA2 (\x y -> [x,y]) (char '\\') anyChar

pIdent :: Parser String
pIdent = liftA2 (:) letter $ many alphaNum

pValue :: Parser Value
pValue = pNum <|> (Quoted <$> pQuoted) <|> (Ident <$> pIdent)

pLine :: Parser (Value, [Value])
pLine = liftA2 (,) pLeft pRight where
  pLeft = lexeme pValue <* lexeme (char '=')
  pRight = lexeme $ many1 $ pValue <* skipMany (oneOf " \t")

pChunk :: Parser (String, RawChunk)
pChunk = liftA2 (,) pName pLines where
  pName = lexeme (char '[') *> lexeme pIdent <* lexeme (char ']')
  pLines = lexeme (char '{') *> many pLine <* lexeme (char '}')
