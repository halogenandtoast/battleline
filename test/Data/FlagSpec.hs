module Data.FlagSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Flag
import Data.Player

main :: IO ()
main = hspec spec

asFlags :: [FlagWinner] -> [Flag]
asFlags = map (Flag [] [])

spec :: Spec
spec = do
  describe "threeInARowWon" $ do
    it "is true when three flags in a row are won" $ do
      threeInARowWon (asFlags [None, Player1, Player1, Player1, Player2]) `shouldBe` True
      threeInARowWon (asFlags [Player1, None, Player1, Player1, Player2]) `shouldBe` False
  describe "wonFive" $ do
    it "is true when five total flags are won" $ do
      wonFive (asFlags [Player1, Player1, Player1, Player1, Player1]) `shouldBe` True
      wonFive (asFlags [Player1, Player1, Player2, Player1, Player1]) `shouldBe` False
      
  
