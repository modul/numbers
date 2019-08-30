{-# LANGUAGE OverloadedStrings #-}

module Api (
  -- * Data types
  -- ** Search argument
  Month, Day, Number (..), Date (..), RangeElement (..),
  -- ** Options
  ApiOptions, ApiOption (..), NotFoundAction (..),
  -- * General API methods
  random, randomWith,
  trivia, triviaWith,
  math, mathWith,
  year, yearWith,
  date, dateWith,
  -- * Convenience Methods
  -- ** Getting specific entries
  triviaFact, mathFact, yearFact, dateFact, dayOfYearFact,
  -- ** Getting random entries
  triviaRandom, mathRandom, yearRandom, dateRandom,
) where

import Network.HTTP.Simple
import Data.Function ((&))
import Data.List (intercalate)
import Data.ByteString.Char8 (pack, unpack, ByteString)

apiHost :: ByteString
apiHost = "numbersapi.com"

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

-- | API options that can be used with any '…With' method
type ApiOptions = [ApiOption]

-- | Possible API options that can be used on most endpoints
data ApiOption = Fragment -- ^ returns the fact as a sentence fragment
               | DefaultMsg ByteString -- ^ sets the default response message if requested number has no entry
               | NotFound NotFoundAction -- ^ selects an alternative action if requested number has no entry
               | MinLimit Int -- ^ lower limit (inclusive) when requesting random values
               | MaxLimit Int -- ^ upper limit (inclusive) when requesting random values
               deriving Show

-- | Describes an alternative action if a requested number has no entry
data NotFoundAction = CeilIfNotFound -- ^ use the next higher number which has an entry
                    | FloorIfNotFound -- ^ use the next lower number which has an entry
                    | DefaultIfNotFound -- ^ return the default response

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

instance Show NotFoundAction where
  show CeilIfNotFound = "ceil"
  show FloorIfNotFound = "floor"
  show DefaultIfNotFound = "default"

-- | By default, no API options are used
defaultApiOptions :: ApiOptions
defaultApiOptions = []

-- | Helper function to show a value as BS.ByteString
bshow :: Show a => a -> ByteString
bshow = pack . show

-- | Builds a key-value-pair from an API option
optionToQueryItem :: ApiOption -> QueryItem
optionToQueryItem  Fragment      = ("fragment", Nothing)
optionToQueryItem (DefaultMsg m) = ("default", Just m)
optionToQueryItem (NotFound   a) = ("notfound", Just $ bshow a)
optionToQueryItem (MinLimit   x) = ("min", Just $ bshow x)
optionToQueryItem (MaxLimit   x) = ("max", Just $ bshow x)

-- | Builds a query string from given API options
optionsToQuery :: ApiOptions -> Query
optionsToQuery = map optionToQueryItem

-- | Builds an HTTP request with given API options and endpoint description
makeApiRequestWith :: ApiOptions -> ApiEndpoint -> Request
makeApiRequestWith opts ep = defaultRequest
                           & setRequestHost apiHost
                           & setRequestPath (bshow ep)
                           & addToRequestQueryString (optionsToQuery opts)

-- | Actually make a request to given endpoint using given options
apiCallWith :: ApiOptions -> ApiEndpoint -> IO String
apiCallWith opts e = do
  let request = makeApiRequestWith opts e
  r <- httpBS request
  return . unpack . getResponseBody $ r

-- | Actually make a request to given endpoint using defaultApiOptions
apiCall :: ApiEndpoint -> IO String
apiCall = apiCallWith defaultApiOptions

-- * General API methods

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

-- | Retrieve a fact about the given day of (any) year
dayOfYearFact :: Day -> IO String
dayOfYearFact = date . DayOfYear
