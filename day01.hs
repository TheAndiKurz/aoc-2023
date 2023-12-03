module Main where
import Data.Char (isDigit)

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
parseWrittenNumber ('o' : 'n' : 'e': rest) = Just 1
parseWrittenNumber ('t' : 'w' : 'o': rest) = Just 2
parseWrittenNumber ('t' : 'h' : 'r' : 'e' : 'e': rest) = Just 3
parseWrittenNumber ('f' : 'o' : 'u' : 'r': rest) = Just 4
parseWrittenNumber ('f' : 'i' : 'v' : 'e': rest) = Just 5
parseWrittenNumber ('s' : 'i' : 'x' : rest) = Just 6
parseWrittenNumber ('s' : 'e' : 'v' : 'e' : 'n': rest) = Just 7
parseWrittenNumber ('e' : 'i' : 'g' : 'h' : 't': rest) = Just 8
parseWrittenNumber ('n' : 'i' : 'n' : 'e': rest) = Just 9
parseWrittenNumber _ = Nothing

main :: IO ()
main = do
    contents <- readFile "day01.data" 
    let numbers = recoverAll (lines contents)
    print (sum numbers)
    
    let numbers = recoverAll2 (lines contents)
    print (sum numbers)
