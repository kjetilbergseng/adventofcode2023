import Day5

main :: IO ()
main = do
    putStrLn "Tests"

    putStrLn $ if getIntersectionPoints [52, 50, 48] (79, 14) == [(81, 14)] 
        then "getIntersectionPoints a OK" else "Failed getIntersectionPoints!"

    putStrLn $ if getIntersectionPoints [52, 50, 48] (48, 10) == [(48, 2), (52, 8)] 
            then "getIntersectionPoints a OK" else "Failed getIntersectionPoints!"
   
    putStrLn $ if getIntersectionPoints [52, 50, 8] (52, 10) == [(54, 6), (58, 4)] 
        then "getIntersectionPoints a OK" else "Failed getIntersectionPoints!" 
        
    putStrLn $ if getIntersectionPoints [52, 11, 10] (5, 20) == [(5, 6), (52, 10), (21, 4)] 
        then "getIntersectionPoints a OK" else "Failed getIntersectionPoints!" 

    return ()