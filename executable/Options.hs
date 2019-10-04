{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Options (
    getOptions,
    run
) where

import System.Console.CmdArgs.Implicit

import Data.Numbers.Api
import Data.Numbers.Input
import Data.Numbers.Client 

import Data.ByteString.Char8 (pack)
import Data.Maybe (catMaybes)

deriving instance Data ApiOption
deriving instance Data Number
deriving instance Data RangeItem
deriving instance Data Date
deriving instance Data NotFoundAction

-- | Available categories: any, trivia, math, year, date
data Category = A | T | M | Y | D deriving (Show, Data)

-- | Wrapped commandline options
data Options = Options Category String [ApiOption]

-- | All available commandline options
data Numbers = Numbers {
        category :: Category,
        fragment :: Maybe Bool,
        defaultMsg :: Maybe String,
        notFound :: Maybe NotFoundAction,
        minLimit :: Maybe Int,
        maxLimit :: Maybe Int,
        value :: String
     } deriving (Data, Typeable, Show)

numbers :: Numbers
numbers = Numbers {
        category = enum [
                A &= ignore &= help "Choose a category (Trivia or Date) based on argument type" ,
                T &= help "Get a trivia entry (default)" &= name "trivia",
                M &= help "Get a math entry" &= name "math",
                Y &= help "Get a year entry" &= name "year",
                D &= help "Get a date entry" &= name "date"
            ] &= groupname "Categories",
        fragment   = Nothing &= help "Return the entry as a sentence fragment" &= groupname "Response options",
        defaultMsg = Nothing &= help "Show this message if requested number has no entry" &= name "default" &= explicit &= typ "MESSAGE" &= groupname "Response options",
        notFound   = Nothing &= help "Selects an alternative if requested number has no entry" &= typ "CEIL | FLOOR | DEFAULT" &= groupname "Lookup options",
        minLimit   = Nothing &= help "Lower limit for random entries" &= name "min" &= explicit &= groupname "Lookup options",
        maxLimit   = Nothing &= help "Upper limit for random entries" &= name "max" &= explicit &= groupname "Lookup options",
        value = def &= args &= typ "NUMBER | DATE"
        } &= helpArg [explicit, name "help", name "h", groupname "Common flags"]
          &= versionArg [explicit, name "version", name "v", groupname "Common flags"]

getOptions :: IO Options
getOptions = do
    opts@Numbers {..} <- cmdArgs numbers
    return . Options category value . getApiOptions $ opts

getApiOptions :: Numbers -> [ApiOption]
getApiOptions Numbers{..} = catMaybes options
    where options = [fragment >>= \p -> if p then Just Fragment else Nothing
                  ,  DefaultMsg . pack <$> defaultMsg
                  ,  NotFound <$> notFound
                  ,  MinLimit <$> minLimit
                  ,  MaxLimit <$> maxLimit]

parseAndGet :: Options -> Either String (IO String)
parseAndGet (Options D val api) = dateWith api <$> parseDate val
parseAndGet (Options T val api) = triviaWith api <$> parseNumber val
parseAndGet (Options M val api) = mathWith api <$> parseNumber val
parseAndGet (Options Y val api) = yearWith api <$> parseNumber val
parseAndGet (Options A val api) = get <$> tryParse val
    where get (IsNumber n) = triviaWith api n
          get (IsDate   n) = dateWith api n

run :: Options -> IO String
run opts@(Options _ val _) = case parseAndGet opts of
                                    Left _ -> return ("Invalid argument: " ++ val)
                                    Right result -> result