{-|
Module      : Client
Description : Client interface for numbersapi.com

numbersapi.com is a trivia database for numbers and dates. This module implements
the client interface to retrieve entries from that service.
-}
module Data.Numbers.Client (
    -- * Lookup methods
    random, randomWith,
    trivia, triviaWith,
    math, mathWith,
    year, yearWith,
    date, dateWith,
    -- * Convenience functions
    -- ** Getting specific entries
    triviaFact, mathFact, yearFact, dateFact,
    -- ** Getting random entries
    triviaRandom, mathRandom, yearRandom, dateRandom,
) where

import Data.Numbers.Api

-- * Lookup methods

-- | Retrieve a random fact
random :: IO String
random = apiCall GetRandom

-- | Same as 'random' that sets optional parameters
randomWith :: ApiOptions -> IO String
randomWith opts = apiCallWith opts GetRandom

-- | Retrieve a trivia entry about the given argument
trivia :: Number -> IO String
trivia = apiCall . GetTrivia

-- | Same as 'trivia' that sets optional parameters
triviaWith :: ApiOptions -> Number -> IO String
triviaWith opts = apiCallWith opts . GetTrivia

-- | Retrieve a math entry about the given argument
math :: Number -> IO String
math = apiCall . GetMath

-- | Same as 'math' that sets optional parameters
mathWith :: ApiOptions -> Number -> IO String
mathWith opts = apiCallWith opts . GetMath

-- | Retrieve a year entry about the given argument
year :: Number -> IO String
year = apiCall . GetYear

-- | Same as 'year' that sets optional parameters
yearWith :: ApiOptions -> Number -> IO String
yearWith opts = apiCallWith opts . GetYear

-- | Retrieve a date entry about the given argument
date :: Date -> IO String
date = apiCall . GetDate

-- | Same as 'date' that sets optional parameters
dateWith :: ApiOptions -> Date -> IO String
dateWith opts = apiCallWith opts . GetDate

-- * Convenience functions
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

-- | Retrieve a fact about the given date
dateFact :: (Month, Day) -> IO String
dateFact = date . uncurry Date

-- ** Getting random entries
-- | Retrieve a fact about a random year
yearRandom :: IO String
yearRandom = year RandomNumber

-- | Retrieve a fact about a random date
dateRandom :: IO String
dateRandom = date RandomDate
