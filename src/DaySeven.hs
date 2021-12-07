-- |

module DaySeven where
import Data.Map (Map, toList, insertWith)

countPositions :: [Int] -> Map Int Int
countPositions = foldl (\m x -> insertWith (+) x 1 m) mempty

findOptimalFuel :: Map Int Int -> Int
findOptimalFuel m = minimum $ map (`calcFuel` m) $ meanKeys m

calcFuel :: Int -> Map Int Int -> Int
calcFuel x = foldl (\total (pos,freq) -> total + (sumOfNats (abs (pos - x)) * freq)) 0 . toList

-- | the sum of natural numbers
sumOfNats :: Int -> Int
sumOfNats n =  ((n*n) + n) `div` 2

meanKeys :: Map Int Int -> [Int]
meanKeys m = [floor mean, ceiling mean]
  where
    (amount,total) = foldr (\(k,x) (a,t) -> (x + a, (k*x) + t)) (0,0) $ toList m
    mean = fromIntegral total / fromIntegral amount :: Double

examineFile :: String -> IO Int
examineFile xs = do
  positions <- countPositions . (\x -> read $ "["++x++"]") <$> readFile xs
  return $ findOptimalFuel positions
