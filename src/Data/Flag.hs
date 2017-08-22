module Data.Flag where

import Data.Card
import Data.List (intercalate)
import Data.Formation

data FlagWinner = None | Player1 | Player2 deriving (Show, Eq)

data Flag = Flag { player1Cards :: [Card]
                 , player2Cards :: [Card]
                 , flagWinner :: FlagWinner
                 }

instance Show Flag where
  show (Flag leftCards rightCards winner) = intercalate ", " (map show leftCards) ++
    " [" ++ show winner ++ "] " ++
    intercalate ", " (map show rightCards)

playToSide :: Int -> Card -> Flag -> Flag
playToSide 1 c f@Flag{player1Cards=cards} = f { player1Cards = c : cards }
playToSide 2 c f@Flag{player2Cards=cards} = f { player2Cards = c : cards }
playToSide _ _ _ = error "Invalid flag side"

updateFlagWinners :: [Flag] -> [Flag]
updateFlagWinners fs = map (updateFlagWinner privateCards) fs
  where
    publicCards = foldr (\f a -> a ++ player1Cards f ++ player2Cards f) [] fs
    privateCards = filter (`notElem` publicCards) mkDeck

updateFlagWinner :: [Card] -> Flag -> Flag
updateFlagWinner cs f
  | length p1 == 3 && length p2 == 3 = if p1f < p2f
                                          then f { flagWinner = Player2 }
                                          else f { flagWinner = Player1 }
  | length p1 < 3 && length p2 < 3 = f
  | length p1 < 3 = if findBestFormation cs p1 <= p2f
                       then f { flagWinner = Player2 }
                       else f
  | otherwise = if findBestFormation cs p2 <= p1f
                   then f { flagWinner = Player1 }
                   else f
  where
    p1 = player1Cards f
    p2 = player2Cards f
    p1f = mkFormation p1
    p2f = mkFormation p2

flagWon :: Flag -> Bool
flagWon (Flag _ _ None) = False
flagWon Flag{} = True

threeInARowWon :: [Flag] -> Bool
threeInARowWon fs = threeInARowWinner fs /= None

threeInARowWinner :: [Flag] -> FlagWinner
threeInARowWinner = go . map flagWinner
  where
    go (x:y:z:xs) = if x == y && y == z && x /= None then x else go (y:z:xs)
    go _ = None

wonFive :: [Flag] -> Bool
wonFive fs = wonFiveWinner fs /= None

wonFiveWinner :: [Flag] -> FlagWinner
wonFiveWinner fs
  | takenCount Player1 == 5 = Player1
  | takenCount Player2 == 5 = Player2
  | otherwise = None
 where
  takenCount player = length . take 5 . filter ((== player) . flagWinner) $ fs
