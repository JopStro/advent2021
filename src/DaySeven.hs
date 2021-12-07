-- |

module DaySeven where
import Data.List (sort)

findOptimalFuelp2 :: [Int] -> Int
findOptimalFuelp2 xs = (`calcFuel` xs) $ meanKey xs
  where
    calcFuel = calcFuelBy sumOfNats

findOptimalFuelp1 :: [Int] -> Int
findOptimalFuelp1 xs = (`calcFuel` xs) $ medianKey xs
  where
    calcFuel = calcFuelBy id

calcFuelBy :: (Int -> Int) -> Int -> [Int] -> Int
calcFuelBy f x = foldl (\total pos -> total + f (abs (pos - x))) 0

-- | the sum of natural numbers
sumOfNats :: Int -> Int
sumOfNats n =  ((n*n) + n) `div` 2

meanKey :: [Int] -> Int
meanKey xs = round (fromIntegral (sum xs) / fromIntegral (length xs))

medianKey :: [Int] -> Int
medianKey xs = sort xs !! floor (fromIntegral (length xs) / 2 :: Double)


examineFile :: String -> IO [Int]
examineFile xs = do
  positions <- (\x -> read $ "["++x++"]") <$> readFile xs
  return $ map (\f -> f positions) [findOptimalFuelp1, findOptimalFuelp2]
