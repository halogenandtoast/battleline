module Data.Player where

import Data.Card
import Data.Sequence ((<|), index, deleteAt, fromList)

data Player = Player { playerName :: String, playerHand :: Hand }

mkPlayer :: String -> [Card] -> Player
mkPlayer name hand = Player name $ fromList hand

giveCard :: Card -> Player -> Player
giveCard card p@Player{playerHand=hand} = p { playerHand = card <| hand }

playCard :: Int -> Player -> (Card, Player)
playCard i p@Player{playerHand=hand} = (card, p { playerHand = hand' })
  where
    card = index hand i
    hand' = deleteAt i hand
