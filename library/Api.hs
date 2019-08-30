{-# LANGUAGE OverloadedStrings #-}

module Api (
  -- * Data types
  Month, Day, Number (..), Date (..), RangeElement (..),
  -- * General API methods
  random, trivia, math, year, date,
  -- * Convenience Methods
  -- ** Getting specific entries
  triviaFact, mathFact, yearFact, dateFact, dayOfYearFact,
  -- ** Getting random entries
  triviaRandom, mathRandom, yearRandom, dateRandom,
) where

import Control.Lens
import Network.Wreq
import Data.List (intercalate)
import Data.Function ((&))
import qualified Data.ByteString.Lazy.Char8 as L8

apiBaseUrl :: String
apiBaseUrl = "http://numbersapi.com"

-- * Data types

type Month = Int
type Day = Int

-- | Describes a numerical argument used in API calls
data Number = Number Int -- ^ a single integer
            | Range [RangeElement] -- ^ a range of integers
            | RandomNumber -- ^ let the API choose a random integer

-- | Describes a date argument used in API calls
data Date = Date Month Day
          | DayOfYear Day
          | RandomDate -- ^ let the API choose a random date

-- | Describes part of a numerical range
data RangeElement = Single Int -- ^ single integer
                  | Interval Int Int -- ^ a closed interval from a to b

-- | Describes all available endpoints with its arguments
data ApiEndpoint = GetTrivia Number
                 | GetMath Number
                 | GetYear Number
                 | GetDate Date
                 | GetRandom

instance Show Number where
  show (Number x) = show x
  show (Range xs) = show <$> xs & intercalate ","
  show RandomNumber = "random"

instance Show Date where
  show (Date m d) = show m ++ "/" ++ show d
  show (DayOfYear d) = show d
  show RandomDate = "random"

instance Show RangeElement where
  show (Single x) = show x
  show (Interval a b) = show a ++ ".." ++ show b

instance Show ApiEndpoint where
  show (GetTrivia x) = show x ++ "/trivia"
  show (GetMath   x) = show x ++ "/math"
  show (GetYear   x) = show x ++ "/year"
  show (GetDate   d) = show d ++ "/date"
  show  GetRandom    = "random"

urlJoin :: [String] -> String
urlJoin = intercalate "/"

apiCall :: ApiEndpoint -> IO String
apiCall e = do
  r <- get url
  let result = r ^. responseBody
  return . L8.unpack $ result
  where url = urlJoin [apiBaseUrl, show e]

-- * General API methods

-- | Retrieve a random fact
random :: IO String
random = apiCall GetRandom

-- | Retrieve a trivia entry about the given argument
trivia :: Number -> IO String
trivia = apiCall . GetTrivia

-- | Retrieve a math entry about the given argument
math :: Number -> IO String
math = apiCall . GetMath

-- | Retrieve a year entry about the given argument
year :: Number -> IO String
year = apiCall . GetYear

-- | Retrieve a date entry about the given argument
date :: Date -> IO String
date = apiCall . GetDate

-- * Convenience methods
-- ** Getting specific entries
-- | Retrieve a trivia fact about the given integer
triviaFact :: Int -> IO String
triviaFact = trivia . Number

-- | Retrieve a trivia fact about a random integer
triviaRandom :: IO String
triviaRandom = trivia RandomNumber

-- | Retrieve a math fact about the given integer
mathFact :: Int -> IO String
mathFact = math . Number

-- | Retrieve a math fact about a random integer
mathRandom :: IO String
mathRandom = math RandomNumber

-- | Retrieve a fact about the given year
yearFact :: Int -> IO String
yearFact = year . Number

-- ** Getting random entries
-- | Retrieve a fact about a random year
yearRandom :: IO String
yearRandom = year RandomNumber

-- | Retrieve a fact about the given date
dateFact :: (Month, Day) -> IO String
dateFact = date . uncurry Date

-- | Retrieve a fact about a random date
dateRandom :: IO String
dateRandom = date RandomDate

-- | Retrieve a fact about the given day of (any) year
dayOfYearFact :: Day -> IO String
dayOfYearFact = date . DayOfYear
