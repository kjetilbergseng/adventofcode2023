module Day7 where
import UtilityFunctions(readInt)
import Data.List (group, sort, sortBy)
import Data.Ord (comparing)

parseHands :: [[Char]] -> [[Char]]
parseHands = map (takeWhile (/= ' ')) 

parseBid :: [[Char]] -> [Int]
parseBid = map (readInt . dropWhile (/= ' '))


findMultipes :: [[Char]] -> [Int]
findMultipes  = map ((10000000000 *) . sum . zipWith (*) [100, 10, 1] . reverse  . sort . map length . group . sort)

highCardValue :: Int -> [Char] -> Int
highCardValue jVal hand= sum $ zipWith (*) [100000000, 1000000, 10000, 100, 1] (map (toNumber jVal) hand)

toNumber ::  Int -> Char -> Int
toNumber jVal c
    | c == 'A' = 14 
    | c == 'K' = 13 
    | c == 'Q' = 12 
    | c == 'J' = jVal
    | c == 'T' = 10
    | otherwise = readInt [c]

handValue :: (Char -> Bool) -> Int -> [[Char]] -> [Int]
handValue pred jVal hand= zipWith (+) (map (highCardValue jVal) hand) (findMultipes $ map (filter pred) hand)

jokerHandValue :: [[Char]] -> [Int] -> [Int]
jokerHandValue hand joker = zipWith (+) (handValue (/= 'J') 1 hand) (map (1000000000000 *) joker)

sortByValue :: Ord a => [(a, b)] -> [b]
sortByValue x = map snd $ sortBy (comparing fst) x

solveDay7 :: (Num a1, Enum a1, Ord a2) => [(a2, a1)] -> a1
solveDay7 xs = sum $ zipWith (*) [1..] (map snd $ sortBy (comparing fst) xs)

numberOfJokers :: [Char] -> Int
numberOfJokers = length . filter (== 'J')  

day7 :: IO ()
day7 = do
    putStrLn "day7"
    contents <- readFile "input/day7.txt"
    let hand = parseHands (lines contents)
    let bid = parseBid (lines contents)
    print $ solveDay7 $ zip (handValue (const True) 11 hand) bid
    let jokers= map numberOfJokers hand
    print $ solveDay7 $ zip (jokerHandValue hand jokers) bid