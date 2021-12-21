-- |

module DayTwentyOne where

type Space = Int
type Score = Int
type Player = (Space, Score)
type Rolls = [Int]
data Turn = One | Two
type Game = (Turn,Player,Player,Int,Rolls)

movePiece :: Space -> Int -> Space
movePiece x d = ((x - 1 + d) `mod` 10) + 1

doTurn :: Player -> Rolls -> (Player,Rolls)
doTurn (space,score) rolls = ((newspace,score+newspace),drop 3 rolls)
  where
    newspace = movePiece space $ sum $ take 3 rolls

stepGame :: Game -> Game
stepGame (One,p1,p2,n,rolls) = (Two,np1,p2,n+3,newrolls)
  where
    (np1,newrolls) = doTurn p1 rolls
stepGame (Two,p1,p2,n,rolls) = (One,p1,np2,n+3,newrolls)
  where
    (np2,newrolls) = doTurn p2 rolls

setRolls :: Game -> Rolls -> Game
setRolls (turn,p1,p2,n,_) rolls = (turn,p1,p2,n,rolls)

createUniverses :: [Game] -> [Game]
createUniverses = concatMap (\game -> [setRolls game [a,b,c] | a <- [1..3], b <- [1..3], c <- [1..3]])

play :: Int -> Int -> Int
play one two = (\(_,(_,p1),(_,p2),n,_) -> min p1 p2 * n) $ head $ dropWhile (\(_,(_,p1),(_,p2),_,_) -> p1 < 1000 && p2 < 1000) $ iterate stepGame (One,(one,0),(two,0),0,[1..])

countVictories :: (Int,Int,[Game]) -> (Int,Int,[Game])
countVictories (points1,points2,[]) = (points1,points2,[])
countVictories (points1,points2,game@(_,p1,p2,_,_):xs) | snd p1 >= 21 = (oldpoints1+snd p1,oldpoints2,games)
                                                       | snd p2 >= 21 = (oldpoints1,oldpoints2+snd p2,games)
                                                       | otherwise    = (oldpoints1,oldpoints2,game:games)
  where
    (oldpoints1,oldpoints2,games) = countVictories (points1,points2,xs)

stepMultiverse :: (Int,Int,[Game]) -> (Int,Int,[Game])
stepMultiverse (p1,p2,games) = (p1,p2,map stepGame . createUniverses $ games)

part2 :: Int -> Int -> Int
part2 one two = (\(p1,p2,_) -> max p1 p2) $ head $ dropWhile (\(_,_,games) -> not $ null games) $ iterate (countVictories . stepMultiverse) (0,0,[(One,(0,one),(0,two),0,[])])
