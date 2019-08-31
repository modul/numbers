{-# LANGUAGE OverloadedStrings #-}

-- * Usage should be as follows
--
-- get entry of specific category (trivia, math, year, date):
--  numbers --trivia [NUMBER] 
--  numbers --math [NUMBER]
--  numbers --year [NUMBER]
--  numbers --date [DATE]
-- gets an entry about a random number or date if argument is missing
--
-- getting an entry in a random category:
--  numbers [INT | RANGE | DATE ]
-- gets an entry about a random number in a random category if argument is missing
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

module Arguments where

import Control.Applicative
import Data.Attoparsec.Text hiding (Number)
import Data.Numbers.Client (Number (..), RangeElement (..), Date (..))

-- parsing a number argument

-- fromRight GetRandom $ GetTrivia <$> (parseOnly number "1,2,3..10")

number :: Parser Number
number =  Range  <$> rangeElement `sepBy` ","
      <|> Number <$> decimal

rangeElement :: Parser RangeElement
rangeElement =  Interval <$> decimal <* string ".." <*> decimal
            <|> Single   <$> decimal

-- parsing a date argument
date :: Parser Date
date =  Date <$> decimal <* char '/' <*> decimal
    <|> DayOfYear <$> decimal
