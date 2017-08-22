module Battleline.Card where

import Data.List (nub, sort)
import Data.Sequence (Seq(..))
import System.Random
import qualified System.Random.Shuffle as RS

type Deck = [Card]
type Hand =  Seq Card
data Color = Blue | Red | Green | Orange | Yellow | Purple deriving (Eq, Enum, Show)
data Card = Card { cardColor :: Color
                 , cardValue :: Int
                 } deriving (Eq)


instance Ord Card where
  (Card _ v1) <= (Card _ v2) = v1 <= v2

instance Show Card where
  show (Card color value) = show color ++ " " ++ show value

shuffle :: StdGen -> Deck -> Deck
shuffle gen deck = RS.shuffle' deck (length deck) gen

mkDeck :: Deck
mkDeck = [Card color value | color <- [Blue .. Purple], value <- [1..10]]

draw :: Int -> Deck -> ([Card], Deck)
draw = splitAt

cardsSameColor :: [Card] -> Bool
cardsSameColor (c:cs) = all (== cardColor c) (map cardColor cs)
cardsSameColor [] = True

cardsSameValue :: [Card] -> Bool
cardsSameValue = (== 1) . length . take 2 . nub . map cardValue

cardsConsecutive :: [Card] -> Bool
cardsConsecutive cs = length (take 2 (nub offsets)) == 1
  where
    values = sort (map cardValue cs)
    offsets = zipWith (-) values [1..]

cardsSum :: [Card] -> Int
cardsSum = sum . map cardValue
