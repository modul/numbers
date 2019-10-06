module Input where

import Test.Tasty.Hspec
import Data.Either (isLeft)

import Data.Numbers.Trivia.Input
import Data.Numbers.Trivia.Api

spec_Input :: Spec
spec_Input = parallel $ do
    describe "parseDate" $ do
        it "parses Month/Day" $
            parseDate "9/11" `shouldBe` Right (Date 9 11)
        it "parses Month.Day" $
            parseDate "9/11" `shouldBe` Right (Date 9 11)
        it "parses Month-Day" $
            parseDate "9/11" `shouldBe` Right (Date 9 11)
        it "parses an empty string" $
            parseDate "" `shouldBe` Right RandomDate
        it "does not check bounds" $ do
            parseDate "13/1" `shouldBe` Right (Date 13 1)
            parseDate "1/32" `shouldBe` Right (Date 1 32)
            parseDate "0/0" `shouldBe` Right (Date 0 0)
            parseDate "13/32" `shouldBe` Right (Date 13 32)
        it "does not allow trailing input" $ do
            parseDate "9/11." `shouldSatisfy` isLeft
            parseDate " 9/11" `shouldSatisfy` isLeft
    describe "parseNumber" $ do
        it "parses a single unsigned number" $ do
            parseNumber "5" `shouldBe` Right (Number 5)
            parseNumber "50" `shouldBe` Right (Number 50)
            parseNumber "0" `shouldBe` Right (Number 0)
        it "parses a single interval" $ do
            parseNumber "1..10" `shouldBe` Right (Range [Interval 1 10])
            parseNumber "0..99" `shouldBe` Right (Range [Interval 0 99])
        it "parses a list of numbers" $
            parseNumber "1,2,3" `shouldBe` Right (Range [Single 1, Single 2, Single 3])
        it "parses a list of intervals and numbers" $ do
            parseNumber "1..10,20,30" `shouldBe` Right (Range [Interval 1 10, Single 20, Single 30])
            parseNumber "90,1..10,30" `shouldBe` Right (Range [Single 90, Interval 1 10, Single 30])
            parseNumber "80,90,1..10" `shouldBe` Right (Range [Single 80, Single 90, Interval 1 10])
            parseNumber "80..90,1..10,100" `shouldBe` Right (Range [Interval 80 90, Interval 1 10, Single 100])
        it "parses an empty string" $
            parseNumber "" `shouldBe` Right RandomNumber
        it "does not parse floating point numbers" $
            parseNumber "0.5" `shouldSatisfy` isLeft
        it "does not allow trailing input" $ do
            parseNumber " 1" `shouldSatisfy` isLeft
            parseNumber "1 " `shouldSatisfy` isLeft
        it "does not allow signed numbers" $ do
            parseNumber "+1" `shouldSatisfy` isLeft
            parseNumber "-1" `shouldSatisfy` isLeft
    describe "tryParse" $ do
        it "parses a single number" $
            tryParse "5" `shouldBe` Right (IsNumber (Number 5))
        it "parses an interval" $
            tryParse "1..10" `shouldBe` Right (IsNumber (Range [Interval 1 10]))
        it "parses an empty string (returns RandomNumber)" $
            tryParse "" `shouldBe` Right (IsNumber RandomNumber)
        it "parses a list of intervals and numbers" $
            tryParse "1,10..20,30" `shouldBe` Right (IsNumber (Range [Single 1, Interval 10 20, Single 30]))
        it "parses a date" $
            tryParse "5/20" `shouldBe` Right (IsDate (Date 5 20))
