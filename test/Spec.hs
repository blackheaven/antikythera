module Main (main) where

import Control.Antikythera
import Data.Time
import Data.Time.Format.ISO8601 (ISO8601, iso8601ParseM, iso8601Show)
import GHC.Exts (IsList (fromList))
import GHC.IO (unsafePerformIO)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Antikythera" $ do
    let testFor :: (ISO8601 a) => Periodicity a -> String -> Maybe String
        testFor p x = iso8601Show <$> p.nextPeriod (time x)
        time :: (ISO8601 a) => String -> a
        time = unsafePerformIO . iso8601ParseM
    describe "ZonedTime" $ do
      describe "every 5 minutes" $ do
        let t = testFor @ZonedTime $ every 5 minute
        it "non overflowing" $
          t "2025-03-15T18:50:50+01:00" `shouldBe` Just "2025-03-15T18:55:00+01:00"
        it "overflowing hour" $
          t "2025-03-15T18:56:50+01:00" `shouldBe` Just "2025-03-15T19:00:00+01:00"
        it "overflowing day" $
          t "2025-03-15T23:56:50+01:00" `shouldBe` Just "2025-03-16T00:00:00+01:00"
        it "overflowing month" $
          t "2025-03-31T23:56:50+01:00" `shouldBe` Just "2025-04-01T00:00:00+01:00"
        it "overflowing year" $
          t "2025-12-31T23:56:50+01:00" `shouldBe` Just "2026-01-01T00:00:00+01:00"
      describe "at 5 minutes" $ do
        let t = testFor @ZonedTime $ at 5 minute
        it "non overflowing" $
          t "2025-03-15T18:01:50+01:00" `shouldBe` Just "2025-03-15T18:05:00+01:00"
        it "overflowing hour" $
          t "2025-03-15T18:56:50+01:00" `shouldBe` Just "2025-03-15T19:05:00+01:00"
        it "overflowing day" $
          t "2025-03-15T23:56:50+01:00" `shouldBe` Just "2025-03-16T00:05:00+01:00"
        it "overflowing month" $
          t "2025-03-31T23:56:50+01:00" `shouldBe` Just "2025-04-01T00:05:00+01:00"
        it "overflowing year" $
          t "2025-12-31T23:56:50+01:00" `shouldBe` Just "2026-01-01T00:05:00+01:00"
      describe "ats 5,35 minutes" $ do
        let t = testFor @ZonedTime $ ats (fromList [5, 35]) minute
        it "non overflowing (5)" $
          t "2025-03-15T18:01:50+01:00" `shouldBe` Just "2025-03-15T18:05:00+01:00"
        it "non overflowing (35)" $
          t "2025-03-15T18:11:50+01:00" `shouldBe` Just "2025-03-15T18:35:00+01:00"
        it "overflowing hour" $
          t "2025-03-15T18:56:50+01:00" `shouldBe` Just "2025-03-15T19:05:00+01:00"
        it "overflowing day" $
          t "2025-03-15T23:56:50+01:00" `shouldBe` Just "2025-03-16T00:05:00+01:00"
        it "overflowing month" $
          t "2025-03-31T23:56:50+01:00" `shouldBe` Just "2025-04-01T00:05:00+01:00"
        it "overflowing year" $
          t "2025-12-31T23:56:50+01:00" `shouldBe` Just "2026-01-01T00:05:00+01:00"
      describe "at 5 hour" $ do
        let t = testFor @ZonedTime $ at 5 hour
        it "non overflowing" $
          t "2025-03-15T02:01:50+01:00" `shouldBe` Just "2025-03-15T05:00:00+01:00"
        it "overflowing day" $
          t "2025-03-15T23:56:50+01:00" `shouldBe` Just "2025-03-16T05:00:00+01:00"
        it "overflowing month" $
          t "2025-03-31T23:56:50+01:00" `shouldBe` Just "2025-04-01T05:00:00+01:00"
        it "overflowing year" $
          t "2025-12-31T23:56:50+01:00" `shouldBe` Just "2026-01-01T05:00:00+01:00"
      describe "at Sunday (weekday)" $ do
        let t = testFor @ZonedTime $ at Sunday weekDay
        it "non overflowing" $
          t "2025-03-15T02:01:50+01:00" `shouldBe` Just "2025-03-16T00:00:00+01:00"
        it "overflowing week" $
          t "2025-03-17T23:56:50+01:00" `shouldBe` Just "2025-03-23T00:00:00+01:00"
        it "overflowing month" $
          t "2025-03-31T23:56:50+01:00" `shouldBe` Just "2025-04-06T00:00:00+01:00"
        it "overflowing year" $
          t "2025-12-31T23:56:50+01:00" `shouldBe` Just "2026-01-04T00:00:00+01:00"
      describe "at 5 month" $ do
        let t = testFor @ZonedTime $ at 5 month
        it "non overflowing" $
          t "2025-03-15T02:01:50+01:00" `shouldBe` Just "2025-05-01T00:00:00+01:00"
        it "overflowing year" $
          t "2025-12-31T23:56:50+01:00" `shouldBe` Just "2026-05-01T00:00:00+01:00"
      describe "at 2026 year" $ do
        let t = testFor @ZonedTime $ at 2026 year
        it "non overflowing" $
          t "2025-03-15T02:01:50+01:00" `shouldBe` Just "2026-01-01T00:00:00+01:00"
      describe "at 5 hour .|| at 5 minute" $ do
        let t = testFor @ZonedTimeWrapped (at 5 hour .|| at 5 minute)
        it "non overflowing (minute)" $
          t "2025-03-15T02:01:50+01:00" `shouldBe` Just "2025-03-15T02:05:00+01:00"
        it "non overflowing (hour)" $
          t "2025-03-15T04:06:50+01:00" `shouldBe` Just "2025-03-15T05:00:00+01:00"
        it "overflowing day" $
          t "2025-03-15T23:56:50+01:00" `shouldBe` Just "2025-03-16T00:05:00+01:00"
      describe "at 5 hour .&& at 5 minute" $ do
        let t = testFor @ZonedTimeWrapped (at 5 hour .&& at 5 minute)
        it "non overflowing" $
          t "2025-03-15T02:01:50+01:00" `shouldBe` Just "2025-03-15T05:05:00+01:00"
        it "overflowing day" $
          t "2025-03-15T23:56:50+01:00" `shouldBe` Just "2025-03-16T05:05:00+01:00"
    describe "ZonedTime" $ do
      describe "at 5 hour .&& sinceInclusive" $ do
        let t = testFor @UTCTime (at 5 hour .&& sinceInclusive (addUTCTime nominalDay) (time "2025-03-15T04:05:00Z"))
        it "non overflowing" $
          t "2025-03-15T02:01:50Z" `shouldBe` Just "2025-03-15T05:00:00Z"
        it "overflowing day (at)" $
          t "2025-03-15T23:56:50Z" `shouldBe` Just "2025-03-16T05:00:00Z"
        it "overflowing day (sinceInclusive)" $
          t "2025-03-14T23:56:50Z" `shouldBe` Just "2025-03-15T05:00:00Z"
      describe "at 5 hour .&& untilInclusive" $ do
        let t = testFor @UTCTime (at 5 hour .&& untilInclusive (addUTCTime nominalDay) (time "2025-03-17T04:05:00Z"))
        it "non overflowing" $
          t "2025-03-15T02:01:50Z" `shouldBe` Just "2025-03-15T05:00:00Z"
        it "overflowing day (at)" $
          t "2025-03-15T23:56:50Z" `shouldBe` Just "2025-03-16T05:00:00Z"
        it "overflowing day (untilInclusive)" $
          t "2025-03-16T23:56:50Z" `shouldBe` Nothing
