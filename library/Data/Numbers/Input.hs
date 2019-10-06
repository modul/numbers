{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Input
Description : Utilities to parse numbers, dates and ranges

These functions can be used to parse a specific syntax for numbers, ranges and dates,
which can then be used with lookup methods from "Client".
-}

module Data.Numbers.Input (
    -- * Data types
    InputParser, LookupType (..),
    -- * Parsing an input string into a lookup value
    tryParse, parseNumber, parseDate
) where

import Control.Applicative
import Data.Functor (($>))
import Data.Attoparsec.ByteString.Char8 hiding (Number, number)
import Data.ByteString.Char8 (pack)

import Data.Numbers.Api (Number (..), RangeItem (..), Date (..))

-- | Alias for functions that parse an input string into some useful type
type InputParser a = String -> Either String a

-- | Distinguishes a lookup value
data LookupType = IsNumber Number | IsDate Date deriving (Show, Eq)

-- | Use this to parse a number argument from an input string
--
-- >>> parseNumber "5"
-- Right (Number 5)
--
-- >>> parseNumber "1..5"
-- Right (Range [Interval 1 5])
--
-- >>> parseNumber "1,2,5-10"
-- Right (Range [Single 1, Single 2, Interval 5 10])
--
-- >>> parseNumber ""
-- Right (RandomNumber)
--
parseNumber :: InputParser Number
parseNumber = makeParser number

-- | Use this to parse a date argument from an input string
--
-- >>> parseDate "12/24"
-- Right (Date 12 24)
--
-- >> parseDate "12-24"
-- Right (Date 12 24)
--
-- >>> parseDate ""
-- Right (RandomDate)
--
parseDate :: InputParser Date
parseDate = makeParser date

-- | Use this to parse either a date tuple or a number (if both could be possible)
tryParse :: String -> Either String LookupType
tryParse s =  IsDate <$> parseDateTuple s
          <|> IsNumber <$> parseNumber s

-- | Use this to parse only a date tuple, see dateTuple
parseDateTuple :: InputParser Date
parseDateTuple = makeParser dateTuple

-- | Builds an argument parser
makeParser :: Parser a -> InputParser a
makeParser p = parseOnly p . pack

-- | Parse number as defined in API
number :: Parser Number
number =  endOfInput $> RandomNumber
      <|> Number <$> decimal <* endOfInput
      <|> Range  <$> rangeItem `sepBy1` "," <* endOfInput

-- | Parse a range item as defined in API
rangeItem :: Parser RangeItem
rangeItem =  Interval <$> decimal <* string ".." <*> decimal
         <|> Single   <$> decimal

-- | Parse a date as defined in API
date :: Parser Date
date =  endOfInput $> RandomDate
    <|> dateTuple

-- | Parse a date tuple consisting of month and day, separated by dateSep
dateTuple :: Parser Date
dateTuple =  Date <$> decimal <* dateSep <*> decimal <* endOfInput

-- | Parse possible date separator characters
dateSep :: Parser Char
dateSep = char '-' <|> char '.' <|> char '/'
