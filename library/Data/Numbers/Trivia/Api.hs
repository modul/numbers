{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Api
Description : API definitions for numbersapi.com

This module defines all endpoints and data types used to interface numbersapi.com.
More convenient client methods to retrieve number facts can be found in "Client".
-}
module Data.Numbers.Trivia.Api (
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
import Data.ByteString.Char8 (unpack, ByteString)

import Data.Numbers.Trivia.Api.Options

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
