{-# LANGUAGE OverloadedStrings #-}
module Api where

import Control.Lens
import Network.Wreq
import Data.List (intercalate)
import Data.Function ((&))
import qualified Data.ByteString.Lazy.Char8 as L8

apiBaseUrl :: String
apiBaseUrl = "http://numbersapi.com"

type Month = Int
type Day = Int

data ApiEndpoint = GetTrivia Number
                 | GetMath Number
                 | GetYear Number
                 | GetDate Date
                 | GetRandom

data Number = Number Int
            | Range [RangeElement]
            | RandomNumber

data Date = Date Month Day
          | DayOfYear Day
          | RandomDate

data RangeElement = Single Int
                  | Interval Int Int

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

urlJoin :: [String] -> String
urlJoin = intercalate "/"

instance Show ApiEndpoint where
  show (GetTrivia x) = show x ++ "/trivia"
  show (GetMath   x) = show x ++ "/math"
  show (GetYear   x) = show x ++ "/year"
  show (GetDate   d) = show d ++ "/date"
  show  GetRandom    = "random"

apiCall :: ApiEndpoint -> IO String
apiCall e = do
  r <- get url
  let result = r ^. responseBody
  return . L8.unpack $ result
  where url = urlJoin [apiBaseUrl, show e]


random :: IO String
random = apiCall GetRandom

trivia :: Number -> IO String
trivia = apiCall . GetTrivia

math :: Number -> IO String
math = apiCall . GetMath

year :: Number -> IO String
year = apiCall . GetYear

date :: Date -> IO String
date = apiCall . GetDate


triviaFact :: Int -> IO String
triviaFact = trivia . Number

triviaRandom :: IO String
triviaRandom = trivia RandomNumber

mathFact :: Int -> IO String
mathFact = math . Number

mathRandom :: IO String
mathRandom = math RandomNumber

yearFact :: Int -> IO String
yearFact = year . Number

yearRandom :: IO String
yearRandom = year RandomNumber

dateFact :: (Month, Day) -> IO String
dateFact = date . uncurry Date

dateRandom :: IO String
dateRandom = date RandomDate

dayOfYearFact :: Day -> IO String
dayOfYearFact = date . DayOfYear
