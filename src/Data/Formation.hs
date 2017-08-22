module Data.Formation where

import Data.Card

data Rank = Host | SkirmishLine | Battalion | Phalanx | Wedge deriving (Eq, Ord, Show)

data Formation = Formation { formationRank :: Rank
                           , formationScore :: Int
                           } deriving (Eq, Ord, Show)

isWedge :: [Card] -> Bool
isWedge cs = cardsSameColor cs && cardsConsecutive cs 

isPhalanx :: [Card] -> Bool
isPhalanx = cardsSameValue

isBattalion :: [Card] -> Bool
isBattalion = cardsSameColor

isSkirmishLine :: [Card] -> Bool
isSkirmishLine = cardsConsecutive

findBestFormation :: [Card] -> [Card] -> Formation
findBestFormation private played = maximum [ mkFormation (perm ++ played) | perm <- subsequencesOfSize (3 - length played) private]

subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize n xs = let l = length xs
                          in if n>l then [] else subsequencesBySize xs !! (l-n)
 where
   subsequencesBySize [] = [[[]]]
   subsequencesBySize (y:ys) = let next = subsequencesBySize ys
                             in zipWith (++) ([]:next) (map (map (y:)) next ++ [[]])

formationRankFromCards :: [Card] -> Rank
formationRankFromCards cs
  | isWedge cs = Wedge
  | isPhalanx cs = Phalanx
  | isBattalion cs = Battalion
  | isSkirmishLine cs = SkirmishLine
  | otherwise = Host

mkFormation :: [Card] -> Formation
mkFormation cs = Formation rank score
  where
    score = cardsSum cs
    rank = formationRankFromCards cs
