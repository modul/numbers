{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}

import System.Console.CmdArgs.Implicit

import Data.Numbers.Input
import Data.Numbers.Client
import Data.Numbers.Api

import Data.ByteString.Char8 (pack)
import Data.Maybe (catMaybes)

deriving instance Data ApiOption
deriving instance Data Number
deriving instance Data RangeItem
deriving instance Data Date
deriving instance Data NotFoundAction

data Category = A | T | M | Y | D deriving (Show, Data)

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
                T &= help "Get a trivia fact (default)" &= name "trivia",
                M &= help "Get a math fact" &= name "math",
                Y &= help "Get a year fact" &= name "year",
                D &= help "Get a date fact" &= name "date"
            ] &= groupname "Categories",
        fragment   = Nothing &= help "Return the fact as a sentence fragment" &= groupname "Response options",
        defaultMsg = Nothing &= help "Show this message if requested number has no entry" &= name "default" &= explicit &= typ "MESSAGE" &= groupname "Response options",
        notFound   = Nothing &= help "Selects an alternative if requested number has no entry" &= typ "CEIL | FLOOR | DEFAULT" &= groupname "Lookup options",
        minLimit   = Nothing &= help "Lower limit for random entries" &= name "min" &= explicit &= groupname "Lookup options",
        maxLimit   = Nothing &= help "Upper limit for random entries" &= name "max" &= explicit &= groupname "Lookup options",
        value = def &= args &= typ "NUMBER | DATE"
        } &= helpArg [explicit, name "help", name "h", groupname "Common flags"]
          &= versionArg [explicit, name "version", name "v", groupname "Common flags"]

main :: IO ()
main = do
    opts@Numbers {..} <- cmdArgs numbers
    let apiOpts = getApiOptions opts
    result <- case run apiOpts category value of
                Left _ -> return "Invalid argument"
                Right r -> r
    putStrLn result

run :: [ApiOption] -> Category -> String -> Either String (IO String)
run o D v = dateWith o <$> parseDate v
run o T v = triviaWith o <$> parseNumber v
run o M v = mathWith o <$> parseNumber v
run o Y v = yearWith o <$> parseNumber v
run o A v = get <$> tryParse v
    where get (IsNumber n) = triviaWith o n
          get (IsDate   n) = dateWith o n

getApiOptions :: Numbers -> [ApiOption]
getApiOptions Numbers{..} = catMaybes options
    where options = [fragment >>= \p -> if p then Just Fragment else Nothing
                  ,  DefaultMsg <$> pack <$> defaultMsg
                  ,  NotFound <$> notFound
                  ,  MinLimit <$> minLimit
                  ,  MaxLimit <$> maxLimit]