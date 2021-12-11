
module DayEleven where
import Data.Vector (Vector, fromList, toList, (!), (!?), (//))
import qualified Data.Vector as V
import Data.Char

type Octopus = Int

step :: (Vector (Vector Octopus), Int) -> (Vector (Vector Octopus), Int)
step (state,flashes) = flashall (0,0) (setup,flashes)
  where
    setup = V.map (V.map (+1)) state
    flash (s,f) (x,y) = case (s !? y) >>= (!? x) of
                          Just 0 -> (s,f)
                          Just n -> if n >= 9 then
                                                foldflash (x,y) (s,f)
                                              else (insert2d s (x,y) (n+1),f)
                          Nothing -> (s,f)
    foldflash (x,y) (s,f) = foldl flash (insert2d s (x,y) 0,f+1) [(x+a,y+b) | a <- [(-1)..1]
                                                                                   , b <- [(-1)..1]
                                                                                   , (a,b) /= (0,0)]
    flashall (x,y) (s,f) | y >= V.length s       = (s,f)
                         | x >= V.length (s ! y) = flashall (0,y+1) (s,f)
                         | otherwise             = flashall (x+1,y) $ if (s ! y ! x) > 9
                                                                        then flash (s,f) (x,y)
                                                                        else (s,f)

insert2d :: Vector (Vector a) -> (Int,Int) -> a -> Vector (Vector a)
insert2d v (x,y) n = v // [(y,(v ! y) // [(x, n)])]

isSync :: Vector (Vector Octopus) -> Bool
isSync = all (==0) . concatMap toList . toList

glow :: String -> IO Int
glow xs = do
  initialState <- fromList . map (fromList . map digitToInt) . lines <$> readFile xs
  let states = iterate step (initialState, 0)
  putStr $ unlines $ take 100 $ tail $ map (show . snd) states
  return $ snd (states !! 100)
