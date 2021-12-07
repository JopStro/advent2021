-- |

module DaySeven where
import Data.Map (Map, toList, fromList, insertWith)
import Data.List (sortBy)

countPositions :: [Int] -> Map Int Int
countPositions = foldl (\m x -> insertWith (+) x 1 m) mempty

findOptimalFuelp2 :: Map Int Int -> Int
findOptimalFuelp2 m = (`calcFuel` m) $ meanKey m
  where
    calcFuel = calcFuelBy sumOfNats

findOptimalFuelp1 :: Map Int Int -> Int
findOptimalFuelp1 m = (`calcFuel` m) $ medianKey m
  where
    calcFuel = calcFuelBy id

calcFuelBy :: (Int -> Int) -> Int -> Map Int Int -> Int
calcFuelBy f x = foldl (\total (pos,freq) -> total + (f (abs (pos - x)) * freq)) 0 . toList

-- | the sum of natural numbers
sumOfNats :: Int -> Int
sumOfNats n =  ((n*n) + n) `div` 2

meanKey :: Map Int Int -> Int
meanKey m = round mean
  where
    (amount,total) = foldr (\(k,x) (a,t) -> (x + a, (k*x) + t)) (0,0) $ toList m
    mean = fromIntegral total / fromIntegral amount :: Double

medianKey :: Map Int Int -> Int
medianKey m = index (sortBy (\(k1,_) (k2,_) -> compare k1 k2) $ toList m) $ round middle
  where
    middle = (/2) . fromIntegral $ foldr (\(_,x) -> (x+)) 0 $ toList m :: Double
    index ((k,x):xs) n | n <= x    = k
                       | otherwise = index xs (n - x)
    index _ _ = head . map fst $ toList m


examineFile :: String -> IO [Int]
examineFile xs = do
  positions <- countPositions . (\x -> read $ "["++x++"]") <$> readFile xs
  return $ map (\f -> f positions) [findOptimalFuelp1, findOptimalFuelp2]
