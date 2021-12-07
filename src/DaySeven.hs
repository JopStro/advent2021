-- |

module DaySeven where
import Data.Vector.Unboxed (Vector, fromList, (!))
import qualified Data.Vector.Unboxed as V
import Statistics.Sample (mean)
import Statistics.Function (sort)

findOptimalFuelp2 :: Vector Int -> Int
findOptimalFuelp2 xs = (`calcFuel` xs) $ meanPos xs
  where
    calcFuel = calcFuelBy sumOfNats

findOptimalFuelp1 :: Vector Int -> Int
findOptimalFuelp1 xs = (`calcFuel` xs) $ medianPos xs
  where
    calcFuel = calcFuelBy id

calcFuelBy :: (Int -> Int) -> Int -> Vector Int -> Int
calcFuelBy f x = V.foldl (\total pos -> total + f (abs (pos - x))) 0

-- | the sum of natural numbers
sumOfNats :: Int -> Int
sumOfNats n =  ((n*n) + n) `div` 2

meanPos :: Vector Int -> Int
meanPos = round . mean . V.map fromIntegral

medianPos :: Vector Int -> Int
medianPos = floor . (\v -> sort v ! floor (fromIntegral (V.length v) / 2)) . V.map fromIntegral


examineFile :: String -> IO [Int]
examineFile xs = do
  positions <- fromList . (\x -> read $ "["++x++"]") <$> readFile xs
  return $ map (\f -> f positions) [findOptimalFuelp1,findOptimalFuelp2]
