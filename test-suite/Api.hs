{-# LANGUAGE OverloadedStrings #-}
module Api where

import Test.Tasty.Hspec

import Data.Numbers.Trivia.Api
import Data.Numbers.Trivia.Api.Options

spec_Endpoints :: Spec
spec_Endpoints = parallel $ do
    context "categories" $ do 
        specify "check trivia endpoint" $
            show (GetTrivia (Number 1)) `shouldBe` "1/trivia"
        specify "check math endpoint" $
            show (GetMath (Number 1)) `shouldBe` "1/math"
        specify "check year endpoint" $
            show (GetYear (Number 1)) `shouldBe` "1/year"
        specify "check date endpoint" $
            show (GetDate (Date 1 5)) `shouldBe` "1/5/date"
        specify "check random endpoint" $
            show GetRandom `shouldBe` "random"
    context "numerical arguments" $ do
        specify "integers are represented in decimal notation" $
            show (Number 1234) `shouldBe` "1234"
        specify "ranges are represented as comma-separated lists" $
            show (Range [Single 1, Single 2, Single 3]) `shouldBe` "1,2,3"
        specify "intervals are represented as from..to" $
            show (Range [Interval 1 100]) `shouldBe` "1..100"
        specify "mixed numbers and intervals are still comma-separated" $
            show (Range [Interval 1 100, Single 5, Interval 200 300]) `shouldBe` "1..100,5,200..300"
        specify "a random number is denoted by 'random'" $
            show RandomNumber `shouldBe` "random"
    context "date arguments" $ do
        specify "dates are represented as month/day" $
            show (Date 5 20) `shouldBe` "5/20"
        specify "a random date is denoted by 'random'" $
            show RandomDate `shouldBe` "random"
    context "query options" $ do
        specify "Fragment is a query flag: ?fragment" $
            optionToQueryItem Fragment `shouldBe` ("fragment", Nothing)
        specify "DefaultMsg is a query option: ?default=string" $
            optionToQueryItem (DefaultMsg "string") `shouldBe` ("default", Just "string")
        specify "NotFound is a query option: ?notfound=ceil|floor|default" $ do
            optionToQueryItem (NotFound CeilIfNotFound) `shouldBe` ("notfound", Just "ceil")
            optionToQueryItem (NotFound FloorIfNotFound) `shouldBe` ("notfound", Just "floor")
            optionToQueryItem (NotFound DefaultIfNotFound) `shouldBe` ("notfound", Just "default")
        specify "MinLimit is a query option: ?min=x" $
            optionToQueryItem (MinLimit 5) `shouldBe` ("min", Just "5")
        specify "MaxLimit is a query option: ?max=x" $
            optionToQueryItem (MaxLimit 5) `shouldBe` ("max", Just "5")
        
