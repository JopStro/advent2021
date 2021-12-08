-- |

module DayEight where
import Data.List
import Data.Map (Map, fromList, toList, (!))
import Data.Tuple
import qualified Data.Map as M

filterUniques :: [String] -> [String]
filterUniques = filter ((`elem` [2,3,4,7]) . length)

mapUniques :: [String] -> Map Int String
mapUniques = fromList . foldl (\xs l -> case length l of
                                          2 -> (1, l) : xs
                                          3 -> (7, l) : xs
                                          4 -> (4, l) : xs
                                          7 -> (8, l) : xs
                                          _ -> xs) []

mapNumbers :: [String] -> Map Int String
mapNumbers xs = fst $ foldr mapValue (initialMap, nonUniques) [6,0,9,5,2,3]
  where
    nonUniques = filter ((`elem` [5,6]) . length) xs
    initialMap = mapUniques xs
    mapValue n (m,ys) = (M.insert n newpattern m, delete newpattern ys)
      where
        newpattern = case n of
                       6 -> head $ filter ((==5) . length . (\\ (m ! 1))) $ filter ((==6) . length) ys
                       0 -> head $ filter ((==4) . length . (\\ (m ! 1))) $ filter ((==6) . length) ys
                       9 -> head $ filter ((==2) . length . (\\ (m ! 4))) $ filter ((==6) . length) ys
                       5 -> head $ filter ((==2) . length . (\\ (m ! 4))) $ filter ((==5) . length) ys
                       2 -> head $ filter ((==3) . length . (\\ (m ! 4))) $ filter ((==5) . length) ys
                       3 -> head $ filter ((==3) . length . (\\ (m ! 1))) $ filter ((==5) . length) ys


getDisplay :: [String] -> [String] -> Int
getDisplay xs = read . concatMap (show . (mappedValues!) . sort)
  where
    mappedValues = fromList . map swap . toList $ mapNumbers $ map sort xs

countUniques :: [String] -> [String] -> Int
countUniques us = length . filter ((`elem` map sort (filterUniques us)) . sort)

parseLine :: String -> ([String], [String])
parseLine xs = (words left, words (tail right))
  where
    (left, right) = break (=='|') xs

analyse :: String -> IO Int
analyse xs = do
  input <- map parseLine . lines <$> readFile xs
  return $ sum $ map (uncurry getDisplay) input
