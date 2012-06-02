-- | Parsec 3 parser for FeedBack .chart format
module Data.FeedBack.Parsec where

import Data.FeedBack.Base
import Text.Parsec
import Text.Parsec.String
import qualified Numeric.NonNegative.Wrapper as NN
import Control.Applicative hiding (many, (<|>), optional)
import Control.Monad
import Data.Ratio

lineSpaces :: Parser a
lineSpaces = skipMany $ oneOf " \t"

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

-- | Parses the sign character (if one exists) at the beginning of a number,
-- returning either -1 or +1.
pSign :: Parser NN.Integer
pSign = (char '-' >> return (-1)) <|> (optional (char '+') >> return 1)

-- | Parses a non-negative integer, a simple string of decimal digits.
pInt :: Parser NN.Integer
pInt = NN.fromNumberUnsafe . read <$> many1 digit

-- | Parses the decimal point and following sequence of numbers, returning that
-- part as a rational number.
pDecimal :: Parser Rational
pDecimal = do
  _ <- char '.'
  dgts <- many1 digit
  return $ read dgts % (10 ^ length dgts)

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
pQuoted = char '"' *> many quotedChar <* char '"' where
  quotedChar, normalChar, escapedChar :: Parser Char
  quotedChar = normalChar <|> escapedChar
  normalChar = noneOf "\"\\"
  escapedChar = char '\\' >> fmap quote anyChar
  quote c = read ['\'', '\\', c, '\'']

pIdent :: Parser String
pIdent = liftA2 (:) letter $ many alphaNum

pValue :: Parser Value
pValue = pNum <|> (Quoted <$> pQuoted) <|> (Ident <$> pIdent)

pLine :: Parser (Value, [Value])
pLine = liftA2 (,) pLeft pRight where
  pLeft = lexeme pValue <* lexeme (char '=')
  pRight = lexeme $ many1 $ pValue <* lineSpaces

pChunk :: Parser (String, RawChunk)
pChunk = liftA2 (,) pName pLines where
  pName = lexeme (char '[') *> lexeme pIdent <* lexeme (char ']')
  pLines = lexeme (char '{') *> many pLine <* lexeme (char '}')
