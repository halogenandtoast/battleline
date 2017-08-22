module Data.FormationSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Formation
import Data.Card

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "compare" $ do
    it "compares values when the rank is the same" $ do
      Formation Host 20 `compare` Formation Host 10 `shouldBe` GT
      Formation Host 10 `compare` Formation Host 20 `shouldBe` LT
      Formation Host 10 `compare` Formation Host 10 `shouldBe` EQ
    it "compares rank when ranks are different" $ do
      Formation Wedge 1 `compare` Formation Phalanx 2 `shouldBe` GT
      Formation Phalanx 1 `compare` Formation Battalion 2 `shouldBe` GT
      Formation Battalion 1 `compare` Formation SkirmishLine 2 `shouldBe` GT
      Formation SkirmishLine 1 `compare` Formation Host 2 `shouldBe` GT
  describe "mkFormation" $ do
    it "returns a wedge if all three cards are the same color and consecutive" $ do
      mkFormation [Card Blue 2, Card Blue 1, Card Blue 3] `shouldBe` Formation Wedge 6
    it "returns a phalanx if all three cards are the same value" $ do
      mkFormation [Card Red 2, Card Blue 2, Card Yellow 2] `shouldBe` Formation Phalanx 6
    it "returns a battalion if all three cards are the same color" $ do
      mkFormation [Card Red 2, Card Red 10, Card Red 5] `shouldBe` Formation Battalion 17
    it "returns a skirmish line if all three cards are consecutive" $ do
      mkFormation [Card Red 2, Card Blue 4, Card Yellow 3] `shouldBe` Formation SkirmishLine 9
    it "returns a host if the other conditions are not met" $ do
      mkFormation [Card Red 2, Card Blue 4, Card Yellow 5] `shouldBe` Formation Host 11
