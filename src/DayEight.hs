-- |

module DayEight (analyse) where
import Data.List
import Data.Tuple
import Data.Map (Map, fromList, toList, (!))
import qualified Data.Map as M
import Data.Set (Set, isSubsetOf)
import qualified Data.Set as S

filterUniques :: [String] -> [String]
filterUniques = filter ((`elem` [2,3,4,7]) . length)

mapUniques :: [Set Char] -> Map Int (Set Char)
mapUniques = fromList . foldl (\xs l -> case S.size l of
                                          2 -> (1, l) : xs
                                          3 -> (7, l) : xs
                                          4 -> (4, l) : xs
                                          7 -> (8, l) : xs
                                          _ -> xs) []

mapNumbers :: [Set Char] -> Map Int (Set Char)
mapNumbers xs = fst $ foldl mapValue (initialMap, nonUniques) [9,0,6,3,5,2]
  where
    nonUniques = filter ((`elem` [5,6]) . S.size) xs
    initialMap = mapUniques xs
    mapValue (m,ys) n = (M.insert n newpattern m, delete newpattern ys)
      where --This is acutally the order they are done in despite being a case statement
        newpattern = case n of
                       9 -> head $ filter ((m ! 4) `isSubsetOf`) $ filter ((==6) . S.size) ys
                       0 -> head $ filter ((m ! 1) `isSubsetOf`) $ filter ((==6) . S.size) ys
                       6 -> head $ filter ((==6) . S.size) ys
                       3 -> head $ filter ((m ! 1) `isSubsetOf`) $ filter ((==5) . S.size) ys
                       5 -> head $ filter (`isSubsetOf` (m ! 6)) $ filter ((==5) . S.size) ys
                       2 -> head $ filter ((==5) . S.size) ys


getDisplay :: [Set Char] -> [Set Char] -> Int
getDisplay xs = read . concatMap (show . (mappedValues!))
  where
    mappedValues = fromList . map swap . toList $ mapNumbers xs

countUniques :: [String] -> [String] -> Int
countUniques us = length . filter ((`elem` map sort (filterUniques us)) . sort)

parseLine :: String -> ([Set Char], [Set Char])
parseLine xs = (map S.fromList $ words left, map S.fromList $ words (tail right))
  where
    (left, right) = break (=='|') xs

analyse :: String -> IO Int
analyse xs = do
  input <- map parseLine . lines <$> readFile xs
  return $ sum $ map (uncurry getDisplay) input
