{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Api
Description : API definitions for numbersapi.com

This module defines all endpoints and data types used to interface numbersapi.com.
More convenient client methods to retrieve number facts can be found in "Client".
-}
module Data.Numbers.Api (
  -- * Data types
  -- ** Search argument
  Number (..), RangeItem (..), Date (..), Month, Day,
  -- ** Lookup options
  ApiOptions, ApiOption (..), NotFoundAction (..),
  -- ** API endpoints
  ApiEndpoint (..),
  -- * API requests
  defaultApiOptions, apiCall, apiCallWith
) where

import Network.HTTP.Simple
import Data.Function ((&))
import Data.List (intercalate)
import Data.ByteString.Char8 (pack, unpack, ByteString)

apiHost :: ByteString
apiHost = "numbersapi.com"

-- * Data types

-- | A month
type Month = Int

-- | A day
type Day = Int

-- | Describes a numerical argument used in API calls
data Number = Number Int -- ^ a single integer
            | Range [RangeItem] -- ^ a range of integers
            | RandomNumber -- ^ let the API choose a random integer
            deriving Eq

-- | Describes a date argument used in API calls
data Date = Date Month Day
          | RandomDate -- ^ let the API choose a random date
          deriving Eq

-- | Describes part of a numerical range
data RangeItem = Single Int -- ^ single integer
               | Interval Int Int -- ^ a closed interval from a to b
               deriving Eq

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
  show RandomDate = "random"

instance Show RangeItem where
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

-- | Send a request to given endpoint using given options
apiCallWith :: ApiOptions -> ApiEndpoint -> IO String
apiCallWith opts e = do
  let request = makeApiRequestWith opts e
  r <- httpBS request
  return . unpack . getResponseBody $ r

-- | Send a request to given endpoint using defaultApiOptions
apiCall :: ApiEndpoint -> IO String
apiCall = apiCallWith defaultApiOptions
