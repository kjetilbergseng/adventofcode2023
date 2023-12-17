module Day5 where
import UtilityFunctions(readInt, split, splitOn)
import Data.Char (isDigit)
import Data.List (nub)

parseSeeds :: String -> [Int]
parseSeeds = map readInt
            . split ' '
            . takeWhile (/= '\n')
            . dropWhile (not . isDigit)

getMap :: [[Char]] -> [[Int]]
getMap =    map (map readInt . split ' ')
            . filter (isDigit . head) 
            . takeWhile (not . null)

getRest :: [String] -> [String]
getRest =   dropWhile null
            . dropWhile (not . null)

applyTuple :: (t -> a, t -> b) -> t -> (a, b)
applyTuple (f, g) x = (f x, g x)


parseMaps :: [[Char]] -> ([[Int]], [String])
parseMaps = applyTuple (getMap, getRest)

applyMap :: [[Int]] -> Int -> Int
applyMap [] n = n
applyMap ([a,b,c]:rs) n
    | n < b || n > b + c = applyMap rs n
    | otherwise = n + a - b

recursiveApplyMap ::  ([[Int]], [String]) -> Int -> Int
recursiveApplyMap (m, []) n = applyMap m n
recursiveApplyMap (m, rs) n =  recursiveApplyMap (parseMaps rs) (applyMap m n)   

day5a :: [[Char]] -> [Int] -> Int
day5a input li = minimum $ map (recursiveApplyMap (parseMaps input)) li
  
seedPairs :: [b] -> [(b, b)]
seedPairs [] = []
seedPairs (x:y:rs) = (x,y) : seedPairs rs

getIntersectionPoints :: [[Int]] -> (Int, Int) -> [(Int, Int)]
getIntersectionPoints [] (s,n) = [(s,n)]
getIntersectionPoints ([a,b,c]:xs) (s,n) 
    | s + n < b                 = getIntersectionPoints xs (s,n)
    | s > b + c                 = getIntersectionPoints xs (s,n)
    | s >= b && s + n <= b + c  = [(s + a -b, n)]
    | s < b && s + n <= b + c   = getIntersectionPoints xs (s, b - s) ++ [(a, n + s - b)]
    | s >= b && s + n > b + c   = getIntersectionPoints xs (b + c, s + n - b - c) ++ 
                                  [(s + a - b, b - s + c)]
    | otherwise           =  getIntersectionPoints xs (s, b-s) ++
                             getIntersectionPoints xs (b + c , s + n - b - c) ++
                             [(a, c)]
    
recursiveGetRanges ::  [[Int]] -> [(Int, Int)] -> [(Int, Int)]
recursiveGetRanges [] _ = []
recursiveGetRanges _ [] = []
recursiveGetRanges x (y:ys) = 
    nub $ 
    getIntersectionPoints x y ++
    recursiveGetRanges x ys


day5b :: ([[Int]], [String]) -> [(Int, Int)] -> [(Int, Int)]
day5b (rs, []) xs = recursiveGetRanges rs xs
day5b (rs, rest) xs = day5b (parseMaps rest) (recursiveGetRanges rs xs)


day5 :: IO ()
day5 = do
    putStrLn "day5"
    contents <- readFile "input/day5.txt"
    
    let seeds = parseSeeds contents
    let (_, rest) = parseMaps $ lines contents
    print $ day5a rest seeds
    print $ minimum $ map fst $ day5b (parseMaps rest) (seedPairs seeds)

