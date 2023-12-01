module Main where

strToInt :: String -> Integer
strToInt [] = 0
strToInt string = read string :: Integer

elvesCarrying :: [String] -> [Integer]
elvesCarrying [str] = [strToInt str]
elvesCarrying (str : strs)
    | null str = 0 : elvesCarrying strs
    | otherwise = strToInt str + head (elvesCarrying strs) : tail (elvesCarrying strs)

maxList :: [Integer] -> Integer
maxList [x] = x
maxList (x : xs) = max x (maxList xs)

max3List :: [Integer] -> (Integer, Integer, Integer)
max3List l = max3Helper l 0 0 0
max3Helper [] m1 m2 m3 = (m1, m2, m3)
max3Helper (x : xs) m1 m2 m3
    | x > m1 = max3Helper xs x m1 m2
    | x > m2 = max3Helper xs m1 x m2
    | x > m3 = max3Helper xs m1 m2 x
    | otherwise = max3Helper xs m1 m2 m3

sum3 (x, y, z) = x + y + z

main :: IO ()
main = do
    contents <- readFile "day01.data" 
    let elves = elvesCarrying (lines contents)
    print (sum3 (max3List elves))
    
