module Data.Game where

import System.Random

import Data.Card
import Data.Player
import Data.Flag
import Control.Lens

data Game = Game { gamePlayer1 :: Player
                 , gamePlayer2 :: Player
                 , gameDeck :: Deck
                 , gameFlags :: [Flag]
                 }

updateGame :: Int -> Int -> Int -> Game -> Game
updateGame playerNum cardNum flagNum game =
  case playerNum of
       1 -> game { gamePlayer1 = player, gameDeck = deck, gameFlags = flags }
       2 -> game { gamePlayer2 = player, gameDeck = deck, gameFlags = flags }
       _ -> error "Invalid player"
  where
    (top:deck) = gameDeck game
    (card, player') = playCard cardNum $ getPlayer playerNum game
    player = giveCard top player'
    flags = updateFlagWinners $ set (element flagNum) flag' $ gameFlags game
    flag' = playToSide playerNum card $ gameFlags game !! flagNum

getPlayer :: Int -> Game -> Player
getPlayer 1 = gamePlayer1
getPlayer 2 = gamePlayer2
getPlayer _ = error "Invalid player"

invalidFlagsFor :: Int -> Game -> [Int]
invalidFlagsFor playerNum game = map snd invalidFlags
  where
    flags = zip (gameFlags game) [1..]
    invalidFlags = filter (\(f, _) -> flagWon f || length (flagPlayerCards f) == 3) flags
    flagPlayerCards = case playerNum of
                           1 -> player1Cards
                           2 -> player2Cards
                           _ -> error "Invalid player"

gameOver :: Game -> Bool
gameOver (Game _ _ _ flags) = threeInARowWon flags || wonFive flags

mkGame :: StdGen -> String -> String -> Game
mkGame gen p1 p2 = Game player1 player2 deck'' flags
  where
    deck = shuffle gen mkDeck
    (hand1, deck') = draw 7 deck
    (hand2, deck'') = draw 7 deck'
    player1 = mkPlayer p1 hand1
    player2 = mkPlayer p2 hand2
    flags = replicate 9 $ Flag [] [] None
