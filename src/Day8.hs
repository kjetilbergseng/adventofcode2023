module Day8 where
import UtilityFunctions(split)
import Data.Char(isAlphaNum)
import Data.Map as Map (fromList, lookup, keys)
import qualified Data.Map as Map
import Debug.Trace (traceShow)
parseDirections :: [Char] -> [Char]
parseDirections = takeWhile (/='\n')

parseNodes :: [[Char]] -> [(Maybe [Char], (Maybe [Char], Maybe [Char]))]
parseNodes li= map ( parseTuple . filter isAlphaNum) (drop 2 li)

parseTuple :: [a] -> (Maybe [a], (Maybe [a], Maybe [a]))
parseTuple li = 
    (Just $ take 3 li, 
    (Just $ take 3 $ drop 3 li, Just $ take 3 $ drop 6 li))

getNumberOfSteps i k m (x:xs)
    |   (Map.lookup k m >>= fn) == Just "ZZZ" = i + 1
    |   otherwise = getNumberOfSteps (i+1) (Map.lookup k m >>= fn) m xs
    where fn = if x=='L' then fst else snd 

getNumberOfStepsB i m (x:xs) k
    |   fmap last k == Just 'Z' = i
    |   otherwise = getNumberOfStepsB (i+1) m xs (Map.lookup k m >>= fn) 
    where fn = if x=='L' then fst else snd 


getKeysThatEndsWithA :: [Maybe [Char]] -> [Maybe [Char]]
getKeysThatEndsWithA = filter (\x -> fmap last x == Just 'A')

day8 :: IO ()
day8 = do
    putStrLn "day8"
    contents <- readFile "input/day8.txt"
    let directions = parseDirections contents
    let nodes = parseNodes $ lines contents
    let nodeMap = Map.fromList nodes
    print $ getNumberOfSteps 0 (Just "AAA") nodeMap (cycle directions)
    let startKeys = getKeysThatEndsWithA $ keys nodeMap
    print $ foldr1 lcm $ map (getNumberOfStepsB 0 nodeMap (cycle directions)) startKeys