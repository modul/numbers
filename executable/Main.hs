{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}

import System.Console.CmdArgs.Implicit

import Arguments
import Data.Numbers.Client 

deriving instance Data ApiOption
deriving instance Data Number
deriving instance Data RangeItem
deriving instance Data Date
deriving instance Data NotFoundAction

data Category = A | T | M | Y | D deriving (Show, Data)

data Numbers = Numbers {
        category :: Category,
        value :: String
     } deriving (Data, Typeable, Show)

numbers :: Numbers
numbers = Numbers {
        category = enum [
                A &= ignore,
                T &= help "Get a trivia fact (default)" &= name "trivia",
                M &= help "Get a math fact" &= name "math",
                Y &= help "Get a year fact" &= name "year",
                D &= help "Get a date fact" &= name "date"
            ] &= groupname "Categories",
        value = def &= args &= typ "NUMBER | DATE"
        } &= helpArg [explicit, name "help", name "h", groupname "Common flags"]
          &= versionArg [explicit, name "version", name "v", groupname "Common flags"]

main :: IO ()
main = do
    Numbers {..} <- cmdArgs numbers
    result <- case run category value of
                Left _ -> return "Invalid argument"
                Right r -> r
    putStrLn result

run :: Category -> String -> Either String (IO String)
run D v  = date   <$> parseDate v
run T v  = trivia <$> parseNumber v
run M v  = math   <$> parseNumber v
run Y v  = year   <$> parseNumber v
run A v  = get <$> tryParse v
    where get (IsNumber n) = trivia n
          get (IsDate   n) = date n