-- |

module DayOne where

import Data.List

floorSteepness :: String -> IO Int
floorSteepness xs = analyse . slide . map read . lines <$> readFile xs

analyse :: [Int] -> Int
analyse [] = 0
analyse [_] = 0
analyse (x:y:xs) | y > x     = 1 + analyse (y:xs)
                 | otherwise = analyse (y:xs)

slide :: [Int] -> [Int]
slide = map sum . unfoldr (\xs -> if null xs then Nothing else Just (take 3 xs, tail xs))
