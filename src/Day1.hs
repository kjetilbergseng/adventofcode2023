module Day1(day1) where
import UtilityFunctions(readInt)
import Data.Char(ord)
import Control.Applicative ( liftA2 )
import Data.List (isPrefixOf, isSuffixOf)

charIsNumber :: Char -> Bool
charIsNumber x=ord x > 47 && ord x < 58

getInts :: [Char] -> [Char]
getInts = filter charIsNumber

addFirstAndLastNumber :: [Char] -> [Char]
addFirstAndLastNumber li = [head li, last li]

findFirstNumber :: [Char] -> Char
findFirstNumber = findNumber isPrefixOf head tail

findLastNumber :: [Char] -> Char
findLastNumber = findNumber isSuffixOf last init

findNumber :: (String -> t -> Bool) -> (t -> Char) -> (t -> t) -> t -> Char
findNumber preSuf headLast tailInit li 
  | charIsNumber $ headLast li = headLast li
  | preSuf "one" li = '1'
  | preSuf "two" li = '2'
  | preSuf "three" li = '3'
  | preSuf "four" li = '4'
  | preSuf "five" li = '5'
  | preSuf "six" li = '6' 
  | preSuf "seven" li = '7' 
  | preSuf "eight" li = '8' 
  | preSuf "nine" li = '9'
  | otherwise = findNumber preSuf headLast tailInit (tailInit li)

combine :: a -> a -> [a]
combine a b = [a,b]

findFirstAndLast :: [[Char]] -> [[Char]]
findFirstAndLast = map $ liftA2 combine findFirstNumber findLastNumber

day1::IO()
day1 = do
  putStrLn "day1"
  contents <- readFile "input/day1.txt"
  let input =  lines contents
  let numbers= map getInts input
  let day1a =  sum $ map (readInt . addFirstAndLastNumber) numbers
  print day1a
  let day1b= sum $ map readInt (findFirstAndLast input)
  print day1b