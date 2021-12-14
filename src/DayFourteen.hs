
module DayFourteen (polymerize) where

import Data.Map (Map, fromList,  fromListWith, toList, (!?), insertWith)
import Data.Bifunctor

step :: Map String Char -> [(String, Int)] -> [(String, Int)]
step _ [] = []
step m ((x,n):xs) = case m !? x of
                      Just ins -> ([head x,ins], n):([ins,last x], n):step m xs
                      Nothing -> (x,n):step m xs

mapPatterns :: String -> Map String Int
mapPatterns [] = mempty
mapPatterns [_] = mempty
mapPatterns (x:y:xs) = insertWith (+) [x,y] 1 $ mapPatterns (y:xs)

mapRules :: [String] -> Map String Char
mapRules = fromList . map ((\xs -> (head xs, head $ last xs)) . words)

-- | don't question it, head doesn't work but last does
countLetters :: [(String,Int)] -> Map Char Int
countLetters = fromListWith (+) . map (first last)

polymerize :: String -> IO (Int,Int)
polymerize xs = do
  (initialState,rules) <- bimap (mapPatterns . head) mapRules . splitAt 2 . lines <$> readFile xs
  let steps = iterate (fromListWith (+) . step rules . toList) initialState
  let frequenciesp1 = map snd . toList . countLetters . toList $ (steps !! 10)
  let frequenciesp2 = map snd . toList . countLetters . toList $ (steps !! 40)
  return (maximum frequenciesp1 -  minimum frequenciesp1,maximum frequenciesp2 - minimum frequenciesp2)
