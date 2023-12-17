module Day4(day4) where

import UtilityFunctions(readInt, split)

score :: Int -> Int
score 0 = 0
score n = last $ take n $ iterate (*2) 1 

parse :: [Char] -> [[Int]]
parse card = map (map readInt . filter (/= "") . split ' ') $ split '|' $ last $ split ':' card

countMatches2 :: [[Int]] -> Int
countMatches2 [x,y]= countMatches x y

countMatches :: [Int] -> [Int] -> Int
countMatches xs ys =  sum $ 0:[ 1 | y<-ys, y `elem` xs ]

day4a :: [[Int]] -> Int
day4a [x,y]= score $ countMatches x y

multipliers :: Num a => [a] -> Int -> Int -> [a]
multipliers xs pos matches = take pos xs ++ map (+ xs!!(pos-1)) (take matches (drop pos xs)) ++ drop (pos + matches) xs

updateMultipliers :: [Int] -> Int -> [Int] -> [Int]
updateMultipliers [x] pos m = multipliers m pos x
updateMultipliers (x:xs) pos m = updateMultipliers xs (pos+1) (multipliers m pos x)

day4 :: IO ()
day4 = do
    putStrLn "day4"
    contents <- readFile "input/day4.txt"
    print $ sum $ map (day4a . parse) (lines contents)

    let matchesList = map (countMatches2 . parse) (lines contents)
    let xs=[1 | _ <-[1..length matchesList]]
    print $ sum $ updateMultipliers matchesList 1 xs