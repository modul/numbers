{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Options
Description : API options and a query builder
-}
module Data.Numbers.Trivia.Api.Options where

import Network.HTTP.Simple
import Data.ByteString.Char8 (pack, ByteString)

-- | Api options that can be used with any '…With' method
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
