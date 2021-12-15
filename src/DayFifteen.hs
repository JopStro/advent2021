{-# LANGUAGE TupleSections #-}

module DayFifteen (pathFile) where
import Data.Vector (Vector, fromList, (!?))
import qualified Data.Vector as V
import Data.Map.Strict (Map, (!), notMember)
import qualified Data.Map as M
import Data.Maybe
import Data.Functor
import Data.Bifunctor
import Data.List
import Data.Char (digitToInt)

type Pos = (Int,Int)
type DijkstraData = (Map Pos Int, Map Pos Int)
type Graph = Vector (Vector Int)

dijkstra :: DijkstraData -> Graph -> Int
dijkstra (spFinal,spMap) v | (V.length v * V.length v) == M.size spFinal = spFinal ! (\n -> (n,n)) (V.length v -1)
                           | M.null spMap = dijkstra (spFinal, M.fromList [((0,0), 0)]) v
                           | otherwise = let ((x,y),sp) = fromMapByPath spMap
                                         in dijkstra (M.insert (x,y) sp spFinal
                                                     , foldl (\m (k,n) -> if k `notMember` spFinal
                                                                          then
                                                                            M.insertWith min k n m
                                                                          else m) (M.delete (x,y) spMap) $ adjacents (x,y) sp v) v


adjacents :: Pos -> Int -> Graph -> [(Pos, Int)]
adjacents (x,y) sp v = catMaybes [(\(x1,y1) -> v !? y1 >>= (!? x1) <&> (+sp) <&> ((x1,y1),))
                                   (a+x,b+y) | a <- [-1..1]
                                             , b <- [-1..1]
                                             , abs a /= abs b]

fromMapByPath :: Map Pos Int -> (Pos,Int)
fromMapByPath = minimumBy (\x y -> compare (snd x) (snd y)) . M.toList



genPart2 :: [[Int]] -> [[Int]]
genPart2 = concat . take 5 . iterate (map (map step)) . map (concat . take 5 . iterate (map step))
  where
    step x | x >= 9 = 1
           | otherwise = x+1

pathFile :: String -> IO (Int,Int)
pathFile xs = (\f -> bimap f f) (dijkstra (mempty,mempty) . fromList . map fromList) . second genPart2 . (\n -> (n,n)) . map (map digitToInt) . lines <$> readFile xs
