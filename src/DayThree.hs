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

oxygen :: [[Int]] -> Integer
oxygen = decimate . head . cull NoInvert 0

co2 :: [[Int]] -> Integer
co2 = decimate . head . cull Invert 0

decimate :: [Int] -> Integer
decimate [] = 0
decimate (x:xs) = unit * fromIntegral x + decimate xs
  where
    unit = 2 ^ length xs

diagnose :: String -> IO (Integer,Integer)
diagnose xs = do
  report <- map (map digitToInt) . lines <$> readFile xs
  let gam = gamma (transpose report)
  let oxy = oxygen report
  let carb = co2 report
  return (decimate gam * decimate (invert gam),oxy * carb)
