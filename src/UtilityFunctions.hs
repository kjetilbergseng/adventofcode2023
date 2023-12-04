module UtilityFunctions where

import Debug.Trace (trace)

chunk :: Int -> [a] -> [[a]]
chunk n ls
  | n <= 0 || null ls = []
  | otherwise = take n ls : chunk n (drop n ls)

split :: Eq a => a -> [a] -> [[a]]
split sep ls
  | null ls = []
  | otherwise = takeWhile (/= sep) ls : split sep (drop 1 $ dropWhile (/= sep) ls)

splitEachChar :: [Char] -> [String]
splitEachChar ls
  | null ls = []
  | otherwise = take 1 ls : splitEachChar (drop 1 ls)

splitOn :: (Foldable t, Eq a) => a -> t [a] -> [[a]]
splitOn sep = concatMap (split sep)

mapReduce :: (a1 -> a2) -> (b -> a2 -> b) -> b -> [a1] -> b
mapReduce transform reduce initial li = foldl reduce initial (map transform li)

readInt :: String -> Int
readInt x = read x :: Int

readInteger :: String -> Integer
readInteger x = read x :: Integer

readCharToInt :: Char -> Int
readCharToInt x = read [x] :: Int

rotate :: Int -> [a] -> [a]
rotate = drop <> take

replace :: Int -> a -> [a] -> [a]
replace n val li = do take n li <> [val] <> drop (n + 1) li

replace2d :: Int -> Int -> a -> [[a]] -> [[a]]
replace2d i j val li = do take i li <> [replace j val (li !! i)] <> drop (i + 1) li

getElement :: Int -> Int -> [[a]] -> a
getElement i j li = (li!!j)!!i

transformElement :: Int -> (t -> t) -> [t] -> [t]
transformElement n fn li = do take n li <> [fn (li !! n)] <> drop (n + 1) li

transformElement2d :: Int -> Int -> (t -> t) -> [[t]] -> [[t]]
transformElement2d i j fn li = do take i li <> [transformElement j fn (li !! i)] <> drop (i + 1) li

(#) :: c -> String -> c
(#) = flip trace

toPair :: [b] -> (b, b)
toPair li = (head li, li !! 1)

ap2 :: (t -> a, t -> b) -> t -> (a, b)
ap2 (a, b) l = (a l, b l) -- ap2 (length, sum) l

apl :: Functor f => f (p -> b) -> p -> f b
apl fa a = fmap ($ a) fa -- apl [length, sum] l

pairMap :: (t -> b) -> (t, t) -> (b, b)
pairMap f (a,b)=(f a, f b)

remove :: (Foldable t, Eq a) => t a -> [a] -> [a]
remove chars=filter (`notElem` chars)

splitPair :: Eq a => a -> [a] -> ([a], [a])
splitPair sep=pairMap (remove [sep]) . span (/= sep)

replaceText :: Eq a => [a] -> [a] -> [a] -> [a]
replaceText [] _ _ = []
replaceText s find repl =
    if take (length find) s == find
        then repl ++ replaceText (drop (length find) s) find repl
        else head s : replaceText (tail s) find repl