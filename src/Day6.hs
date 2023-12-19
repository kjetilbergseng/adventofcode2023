module Day6 where
import UtilityFunctions(readInt, split)
import Data.Char (isDigit)

parse :: [Char] -> [Int]
parse = map readInt . 
        filter (not . null) .
        split ' ' .
        dropWhile (not . isDigit) 

toDouble :: Integral a => a -> Double
toDouble num = fromIntegral num :: Double

parsePartb :: [Char] -> Int
parsePartb = readInt . filter isDigit

solveEq :: Double -> Double -> (Double, Double)
solveEq  t d =   ((t + sqrt (t*t - 4.0 * d))/2.0,  
                  (t - sqrt (t*t - 4.0 * d))/2.0)

findNumberOfWays :: Double -> Double -> Int
findNumberOfWays t d = floor (max a b - tolerance) - ceiling (min a b - 1 + tolerance)
    where (a, b) = solveEq t d
          tolerance = 1e-9

day6 :: IO ()
day6 = do
    putStrLn "day6"
    contents <- readFile "input/day6.txt"

    let times= (map toDouble . parse . head . lines) contents
    let distances= (map toDouble . parse . last . lines) contents
    print $ product $ zipWith findNumberOfWays times distances
    
    let t= toDouble $ (parsePartb . head . lines) contents
    let d= toDouble $ (parsePartb . last . lines) contents
    print $ findNumberOfWays t d
    