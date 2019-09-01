{-# LANGUAGE OverloadedStrings #-}

-- * Usage should be as follows
--
-- get entry of specific category (trivia, math, year, date):
--  numbers --trivia [NUMBER] # same as numbers [NUMBER]
--  numbers --math [NUMBER]
--  numbers --year [NUMBER]
--  numbers --date [DATE]
-- gets an entry about a random number or date if argument is missing
--
-- getting a trivia entry about given number or date
--  numbers [INT | RANGE | DATE ] # same as numbers --trivia
-- gets an entry about a random number if argument is missing
--
-- argument syntax should be:
--  NUMBER: integer | RANGE
--  RANGE: (INT | INT..INT)[,...]
--  DATE: DATETUPLE | DAYOFYEAR
--  DATETUPLE: MONTH/DAY 
--  DAYOFYEAR: integer
--  MONTH: integer
--  DAY: integer
-- 
-- API options:
-- --fragment
-- --min=INT
-- --max=INT
-- --default=STR
-- --notfound=CEIL | FLOOR | DEFAULT  
--
-- Hint: without a category, the argument could be a DATETUPLE or NUMBER

module Arguments (
    parseDate,
    parseDateTuple,
    parseNumber,
    LookupType (..),
    tryParse
) where

import Control.Applicative
import Data.Attoparsec.Text hiding (Number, number)
import Data.Text (pack)
import Data.Numbers.Client (Number (..), RangeItem (..), Date (..))

type ArgumentParser a = String -> Either String a

data LookupType = IsNumber Number | IsDate Date deriving (Show)

-- * Parsing the supplied argument

-- | Use this to parse any number argument from the commandline
parseNumber :: ArgumentParser Number
parseNumber = makeParser number

-- | Use this to parse any date argument from the commandline
parseDate :: ArgumentParser Date
parseDate = makeParser date

-- | Use this to parse only a date in the form of Month/Day
parseDateTuple :: ArgumentParser Date
parseDateTuple = makeParser dateTuple

-- | Use this to parse either a date tuple or a number (if both could be possible)
tryParse :: String -> Either String LookupType
tryParse s =  IsDate <$> parseDateTuple s
          <|> IsNumber <$> parseNumber s

-- | Builds an argument parser
makeParser :: Parser a -> ArgumentParser a    
makeParser p = parseOnly p . pack

-- * Actual parsers

-- | Parse number as defined in API
number :: Parser Number
number =  endOfInput *> pure RandomNumber
      <|> Number <$> decimal <* endOfInput
      <|> Range  <$> rangeItem `sepBy1` "," <* endOfInput

-- | Parse a range item as defined in API
rangeItem :: Parser RangeItem
rangeItem =  Interval <$> decimal <* string ".." <*> decimal
            <|> Single   <$> decimal

-- | Parse a date as defined in API
date :: Parser Date
date =  endOfInput *> pure RandomDate
    <|> dateTuple
    <|> DayOfYear <$> decimal <* endOfInput

-- | Parse a date tuple as defined in API
dateTuple :: Parser Date
dateTuple = Date <$> decimal <* char '/' <*> decimal <* endOfInput
