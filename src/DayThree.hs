-- |

module DayThree (diagnose) where

import Data.List
import Data.Char

gamma :: [[Int]] -> [Int]
gamma []                       = []
gamma (xs:xss) | ones >= zeros = 1 : gamma xss
               | zeros > ones  = 0 : gamma xss
  where
    (ones, zeros) = (sum xs, length xs - sum xs)
gamma _ = []

invert :: [Int] -> [Int]
invert [] = []
invert (0:xs) = 1 : invert xs
invert (_:xs) = 0 : invert xs

data Method = Invert | NoInvert

cull :: Method -> Int -> [[Int]] -> [[Int]]
cull _ _ [xs] = [xs]
cull method i xss | i >= length (head xss) = cull method 0 xss
                  | otherwise = cull method (i+1) $ case method of
                                                      NoInvert -> filter (\xs -> (xs !! i) == filterVal) xss
                                                      Invert -> filter (\xs -> (xs !! i) /= filterVal) xss
                                                where
                                                  filterVal = gamma (transpose xss) !! i

oxygen :: [[Int]] -> Int
oxygen = decimate . head . cull NoInvert 0

co2 :: [[Int]] -> Int
co2 = decimate . head . cull Invert 0

decimate :: [Int] -> Int
decimate [] = 0
decimate (x:xs) = unit * x + decimate xs
  where
    unit = 2 ^ length xs

diagnose :: String -> IO Int
diagnose xs = do
  report <- map (map digitToInt) . lines <$> readFile xs
  let oxy = oxygen report
  let carb = co2 report
  return $ oxy * carb
