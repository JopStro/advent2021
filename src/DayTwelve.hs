{-# LANGUAGE LambdaCase #-}

module DayTwelve where
import Data.Bifunctor
import Data.Map (Map, fromListWith, (!))
import Data.Char
import Data.Tuple
import Data.List

data Node = Start | End | Big String | Small String deriving (Eq,Show,Ord)

stringToNode :: String -> Node
stringToNode "start" = Start
stringToNode "end"   = End
stringToNode xs | isUpper $ head xs = Big xs
                | otherwise = Small xs

mapGraph :: [(String,String)] -> Map Node [Node]
mapGraph = fromListWith (++) . map (second (:[])) . filter (fst . first (/=End)) . doubleUp . map (bimap stringToNode stringToNode)
  where
    doubleUp xs = xs ++ map swap xs

findPaths :: Map Node [Node] -> [Node] -> Bool -> Node -> Int
findPaths _ _ _ End = 1
findPaths m path dups Start | null path = sum $ map (findPaths m path dups) (m ! Start)
                            | otherwise = 0
findPaths m path False n = sum $ map (findPaths m (n:path) False) ((m ! n) \\ filter (\case
                                                                                       Small _ -> True
                                                                                       _ -> False) path)
findPaths m path True n = sum $ map (\case
                                         Small x | Small x `elem` path -> findPaths m (n:path) False (Small x)
                                         x -> findPaths m (n:path) True x) (m ! n)

pathFinding :: String -> IO (Int,Int)
pathFinding xs = do
  graph <- mapGraph . map (second tail . break (=='-')) . lines <$> readFile xs
  return (findPaths graph [] False Start, findPaths graph [] True Start)
