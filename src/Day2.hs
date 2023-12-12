module Day2(day2) where
import UtilityFunctions(readInt, split, replaceText)
import Data.List (isSuffixOf)
import Data.Char (isDigit)

data Game = Game
  { num     :: Int,
    red    :: Int,
    green  :: Int,
    blue   :: Int
  } deriving (Show)

getColorMax :: [Char] -> [[Char]] -> Int
getColorMax color [x] 
    | color `isSuffixOf` x  = readInt $ takeWhile isDigit x
    | otherwise = 0
getColorMax color (x:xs) = max (getColorMax color [x]) (getColorMax color xs)

parseGame :: [Char] -> Game
parseGame li = Game gameNum (getColorMax "red" colorList) (getColorMax "green" colorList) (getColorMax "blue" colorList)
    where
        filtered = filter (/=' ') li
        gameNum = readInt $ takeWhile isDigit (drop 4 filtered)
        colorList = split ';'. replaceText "," ";" $ drop 1 . dropWhile (/= ':') $ filtered

checkRequirement :: Int -> Int -> Int -> Game -> Int
checkRequirement maxRed maxGreen maxBlue (Game n r g b) =      
    if r > maxRed || g > maxGreen || b > maxBlue
    then 0
    else n

gamePpower :: Game -> Int
gamePpower (Game _ r g b) = r * g * b    

day2::IO()
day2 = do
  putStrLn "day2"
  contents <- readFile "input/day2.txt"
  let input =  lines contents
  let game=map parseGame input 
  let day2a= sum $ map (checkRequirement 12 13 14) game
  print day2a
  let day2b= sum $ map gamePpower game
  print day2b