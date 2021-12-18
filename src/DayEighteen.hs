-- |

module DayEighteen where
import Data.List
import Data.Bool
import Data.Char

data SnailNum = P SnailNum SnailNum | N Int deriving (Eq, Ord, Show)
data Dir = L | R deriving (Eq,Ord,Show)
type Path = [Dir]

swapDir :: Dir -> Dir
swapDir L = R
swapDir R = L

getNode :: Dir -> SnailNum -> Int
getNode L (P (N l) _) = l
getNode R (P _ (N r)) = r
getNode _ _ = error "Invalid number"


explode :: SnailNum -> SnailNum -> Path -> Maybe SnailNum
explode _ (N _) _ = Nothing
explode root node@(P (N _) (N _)) p@(_:_:_:_:_) | length (group (sort p)) == 1 = Just $ addPath (zeroPath root p) (reverse (swapDir (head p):tail p)) (swapDir $ last p) $ getNode (swapDir (head p)) node
                                                | head p /= (p !! 1)    = Just $ addPath (addPath (zeroPath root (reverse p)) (reverse (map swapDir (take 2 p) ++ drop 2 p)) (head p) (getNode (head p) node)) (reverse (swapDir (head p):tail p)) (swapDir (head p)) $ getNode (swapDir (head p)) node
                                                | (p !! 1) /= (p !! 2) = Just $ addPath (addPath (zeroPath root (reverse p)) (reverse (map swapDir (take 3 p) ++ drop 3 p)) (head p) (getNode (head p) node)) (reverse (swapDir (head p):tail p)) (swapDir (head p)) $ getNode (swapDir (head p)) node
                                                | (p !! 2) /= (p !! 3) = Just $ addPath (addPath (zeroPath root (reverse p)) (reverse (map swapDir (take 4 p) ++ drop 4 p)) (head p) (getNode (head p) node)) (reverse (swapDir (head p):tail p)) (swapDir (head p)) $ getNode (swapDir (head p)) node
                                                | otherwise = Just $ addPath (addPath (zeroPath root (reverse p)) (reverse (map swapDir p)) (head p) (getNode (head p) node)) (reverse (swapDir (head p):tail p)) (swapDir (head p)) $ getNode (swapDir (head p)) node
explode root (P l r) p = case explode root l (L:p) of
                           Nothing -> explode root r (R:p)
                           x -> x

split :: SnailNum -> Maybe SnailNum
split (N x) | x > 9 = Just $ P (N (x `div` 2)) (N (x - (x `div` 2)))
            | otherwise = Nothing
split (P l r) = case split l of
                  Nothing -> case split r of
                               Nothing -> Nothing
                               Just x -> Just (P l x)
                  Just x -> Just (P x r)


reduce :: SnailNum -> SnailNum
reduce x = maybe (maybe x reduce $ split x) reduce sploded
  where
    sploded = explode x x []

magnitude :: SnailNum -> Int
magnitude (N x) = x
magnitude (P x y) = (3 * magnitude x) + (2 * magnitude y)

zeroPath :: SnailNum -> Path -> SnailNum
zeroPath _ [] = N 0
zeroPath (P l r) (L:p) = P (zeroPath l p) r
zeroPath (P l r) (R:p) = P l (zeroPath r p)
zeroPath _ _ = error "ZeroPath unreachable"

addPath :: SnailNum -> Path -> Dir -> Int -> SnailNum
addPath (N x) _ _ n = N (x+n)
addPath (P l r) (L:p) side n = P (addPath l (bool p (repeat (swapDir side)) (null p)) side n) r
addPath (P l r) (R:p) side n = P l (addPath r (bool p (repeat (swapDir side)) (null p)) side n)
addPath _ _ _ _ = error "addPath unreachable"

parseTree :: String -> SnailNum
parseTree = fst . chopChar
  where
    chopChar ('[':s) = (P l r,rest)
      where
        (l,rs) = chopChar s
        (r,rest) = chopChar rs
    chopChar (x:s) = (N (digitToInt x), dropWhile (`elem` ",]") s)
    chopChar [] = error "no parse"

sumSnails :: String -> IO (Int,Int)
sumSnails xs = do
  nums <- map parseTree . lines <$> readFile xs
  let total = magnitude $ foldl1 (\x y -> reduce $ P x y) nums
      maxmag = maximum [magnitude . reduce $ P a b | a <- nums, b <- nums, a /= b]
  return (total,maxmag)
