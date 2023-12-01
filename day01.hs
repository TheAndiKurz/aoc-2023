module Main where
import Data.Char (isDigit)

strToInt :: String -> Integer
strToInt [] = 0
strToInt string = read string :: Integer

-- part one
recoverAll :: [String] -> [Integer]
recoverAll = map recover

recover :: String -> Integer
recover str = recoverOneDir str * 10 + recoverOneDir (reverse str) 
    where recoverOneDir (c : cs) 
            | isDigit c = read [c]
            | otherwise = recoverOneDir cs


-- part two
recoverAll2 :: [String] -> [Integer]
recoverAll2 = map recover2

recover2 :: String -> Integer
recover2 str = let numsLine = recoverLine str in 
    head numsLine * 10 + last numsLine

recoverLine :: String -> [Integer]
recoverLine [] = []
recoverLine str@(c : cs)
    | isDigit c = read [c] : recoverLine cs
    | otherwise = case parseWrittenNumber str of
                    Just n -> n : recoverLine cs
                    Nothing -> recoverLine cs

parseWrittenNumber :: String -> Maybe Integer
parseWrittenNumber str 
    | strBeginsWith "one" str = Just 1 
    | strBeginsWith "two" str = Just 2
    | strBeginsWith "three" str = Just 3
    | strBeginsWith "four" str = Just 4
    | strBeginsWith "five" str = Just 5
    | strBeginsWith "six" str = Just 6
    | strBeginsWith "seven" str = Just 7
    | strBeginsWith "eight" str = Just 8
    | strBeginsWith "nine" str = Just 9
    | otherwise = Nothing

strBeginsWith :: String -> String -> Bool
strBeginsWith [] [] = True
strBeginsWith [] str = True
strBeginsWith str [] = False
strBeginsWith (c1 : cs1) (c2 : cs2)
    | c1 == c2 = strBeginsWith cs1 cs2
    | otherwise = False

main :: IO ()
main = do
    contents <- readFile "day01.data" 
    let numbers = recoverAll (lines contents)
    print (sum numbers)
    
    let numbers = recoverAll2 (lines contents)
    print (sum numbers)
