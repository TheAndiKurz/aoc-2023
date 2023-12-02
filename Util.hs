module Util where
import Data.Char (isDigit)

printList :: Show a => [a] -> IO ()
printList [] = do return ()
printList (a : as) = do
    print a
    printList as


splitStr :: Char -> String -> [String]
splitStr c1 (c2 : cs) = splitStr' c1 (c2 : cs) ""
    where 
        splitStr' c1 [] acc = [acc]
        splitStr' c1 (c2 : cs) acc
            | c2 == c1 = acc : splitStr' c1 cs ""
            | otherwise = splitStr' c1 cs (acc ++ [c2])

strToInt :: String -> Integer
strToInt [] = 0
strToInt string = read string :: Integer

getNum :: String -> String
getNum [] = []
getNum (c : cs)
    | isDigit c = c : getNum cs
    | otherwise = []
