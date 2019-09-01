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

module Arguments (
    parseDate,
    parseNumber
) where

import Control.Applicative
import Data.Attoparsec.Text hiding (Number, number)
import Data.Text (pack)
import Data.Numbers.Client (Number (..), RangeElement (..), Date (..))

type ArgumentParser a = String -> Either String a

-- use to parse a numererical argument from the commandline
parseNumber :: ArgumentParser Number
parseNumber = makeParser number

-- use to parse a date argument from the commandline
parseDate :: ArgumentParser Date
parseDate = makeParser date

-- build an argument parser
makeParser :: Parser a -> ArgumentParser a    
makeParser p = parseOnly p . pack


number :: Parser Number
number =  Range  <$> rangeElement `sepBy1` ","
      <|> Number <$> decimal

rangeElement :: Parser RangeElement
rangeElement =  Interval <$> decimal <* string ".." <*> decimal
            <|> Single   <$> decimal

date :: Parser Date
date =  Date <$> decimal <* char '/' <*> decimal
    <|> DayOfYear <$> decimal
