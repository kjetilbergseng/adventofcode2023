module Day3(day3) where

import UtilityFunctions (readInt)
import Data.Char (isDigit)

numberPositons :: Int -> Int -> String -> [((Int,Int), Int , Int)]
numberPositons _ _ []=[]
numberPositons line pos li@(x:xs)
    | x == '\n' = numberPositons (line+1) 0 xs
    | isDigit x = 
        ((line, pos), lengthDigits li, readInt $ takeWhile isDigit li)
        : numberPositons line (pos + lengthDigits li) (dropWhile isDigit li)
    | otherwise = numberPositons line (pos+1) xs
    where lengthDigits=length . takeWhile isDigit

mapSymbols :: Int -> Int -> String -> [(Int,Int)]
mapSymbols _ _ []=[]
mapSymbols line pos (x:xs)
    | x == '\n' = nextline
    | isDigit x || x == '.' = nextPos
    | otherwise = add3by3Symbol (line, pos) ++ nextPos
    where nextPos = mapSymbols line (pos+1) xs
          nextline = mapSymbols (line+1) 0 xs

add3by3Symbol :: (Int,Int) -> [(Int,Int)]
add3by3Symbol (x,y)= [ (i,j) | i<-[x-1..x+1],j<-[y-1..y+1]]

day3a :: [(Int,Int)]  -> [((Int,Int), Int , Int)] -> Int -> Int
day3a _ [] _= 0
day3a mapKeys li@(((i,j), l, num):xs) n
    | (i,j+n) `elem` mapKeys = num + day3a mapKeys xs 0
    | n<l-1 =  day3a mapKeys li (n+1)
    | otherwise = day3a mapKeys xs 0

day3b ::  [((Int,Int), Int , Int)] -> Int -> (Int,Int)  -> [Int]
day3b [] _ _= []
day3b li@(((i,j), l, num):xs) n gear
    | (i,j+n) `elem` add3by3Symbol gear = num : day3b xs 0 gear
    | n<l-1 =  day3b li (n+1) gear
    | otherwise = day3b xs 0 gear

mapGears :: Int -> Int -> String -> [(Int,Int)]
mapGears _ _ [] = []
mapGears line pos (x:xs)
    | x == '\n' = mapGears (line+1) 0 xs
    | x == '*' = (line, pos) : mapGears line (pos+1) xs
    | otherwise = mapGears line (pos+1) xs

gearProd :: Num a => [a] -> a
gearProd [x,y] = x*y   
gearProd _ = 0 

day3::IO()
day3 = do
    putStrLn "day3"
    contents <- readFile "input/day3.txt"

    let numPos = numberPositons 0 0  contents 
    let symbols = mapSymbols 0 0 contents  
    print $ day3a symbols numPos 0 
    
    let gears = mapGears 0 0 contents
    let gearComb = map (day3b numPos 0) gears
    print $ sum $ map gearProd gearComb 