-- |

module DaySix where

import Data.IntMap (IntMap, insertWith, toList, fromListWith)

simFile :: String -> IO Integer
simFile xs = do
  contents <- readFile xs
  let list = read $ "["++contents++"]" :: [Int]
  let initialState = mapFish list
  return $ simulate 256 initialState

mapFish :: [Int] -> IntMap Integer
mapFish = foldl (\m x -> insertWith (+) x 1 m) (mempty :: IntMap Integer)

simulate :: Int -> IntMap Integer -> Integer
simulate 0 xs = sum . map snd $ toList xs
simulate n xs = simulate (n-1) $ fromListWith (+) . spawn $ toList xs

spawn :: [(Int,Integer)] -> [(Int,Integer)]
spawn [] = []
spawn ((0,x):xs) = [(8,x),(6,x)] ++ spawn xs
spawn ((x,y):xs) = (x-1,y) : spawn xs
