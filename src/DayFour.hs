-- |

module DayFour where

import Data.List
import Data.Maybe

bingo :: FilePath -> IO (Int,Int)
bingo xs = do
  (rawdraw:rawboards) <- filter (not . null) . lines <$> readFile xs
  let draw = read $ "["++rawdraw++"]"
  let boards = parseBoards rawboards
  let (winningscore:scores) = play draw boards
  return (winningscore, last scores)

parseBoards :: [String] -> [[[Maybe Int]]]
parseBoards = unfoldr (\s -> if null s then Nothing else Just $ splitAt 5 s) . map (map (Just . read) . words)

play :: [Int] -> [[[Maybe Int]]] -> [Int]
play [] _ = []
play (x:xs) boards = map calcScore winners ++ play xs remaining
  where
    newBoards = map (map (map (\y -> if y == Just x then Nothing else y))) boards
    (winners, remaining) = partWinners newBoards
    calcScore = (*x) . sum . concatMap catMaybes

partWinners :: [[[Maybe Int]]] -> ([[[Maybe Int]]], [[[Maybe Int]]])
partWinners = partition check
  where
    check = elem [] . map catMaybes . (id <> transpose)
