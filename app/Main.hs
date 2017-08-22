module Main where

import System.Random
import System.IO
import Text.Read

import Data.Game
import Data.Player (playerHand, playerName)
import Data.Sequence (Seq(..))
import Data.Foldable (toList)

ask :: String -> IO String
ask msg = putStr msg >> hFlush stdout >> getLine

chooseFromSequence :: (Show a) => Seq a -> [Int] -> String -> IO Int
chooseFromSequence = chooseFromArray . toList

chooseFromArray :: (Show a) => [a] -> [Int] -> String -> IO Int
chooseFromArray xs invalid question = do
    putStrLn $ unlines xsWithIndex
    mchoice <- readMaybe <$> ask question
    case mchoice of
         Just x -> if valid x then pure $ x - 1 else redo
         Nothing -> redo
  where
    xsWithIndex = [show i ++ ": " ++ show x | (x, i) <- zip xs ([1..] :: [Int])]
    redo = putStrLn "Invalid choice" >> chooseFromArray xs invalid question
    valid x = x > 0 && x <= length xs && x `notElem` invalid

newGame :: IO Game
newGame = mkGame <$> newStdGen <*> ask "Player 1 name: " <*> ask "Player 2 name: "

playerTurn :: Int -> Game -> IO Game
playerTurn p g = do
    putStrLn $ unlines [show i ++ ": " ++ show x | (x, i) <- zip (gameFlags g) ([1..] :: [Int])]
    putStrLn $ playerName player ++ "'s turn:"
    cardChoice <- chooseFromSequence (playerHand player) [] "Play which card: "
    flagChoice <- chooseFromArray (gameFlags g) (invalidFlagsFor p g) "To which flag: "
    return $ updateGame p cardChoice flagChoice g
  where
    player = getPlayer p g

play :: Game -> IO ()
play = go 1 (go 2 play)
  where
    go n k game = do
      game' <- playerTurn n game
      if gameOver game'
        then putStrLn $ "Player " ++ show n ++ " wins"
        else k game'

main :: IO ()
main = newGame >>= play
