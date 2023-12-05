module Util where
import Data.Char (isDigit)

printList :: Show a => [a] -> IO ()
printList [] = do return ()
printList (a : as) = do
    print a
    printList as


splitStr :: Char -> String -> [String]
splitStr c1 str = splitStr' c1 str ""
    where 
        splitStr' c1 [] acc = [acc]
        splitStr' c1 (c2 : cs) acc
            | c2 == c1 = acc : splitStr' c1 cs ""
            | otherwise = splitStr' c1 cs (acc ++ [c2])

splitString :: String -> String -> [String]
splitString delimiter toSplit =
    let 
        splitString' :: String -> String -> [String]
        splitString' [] acc = [acc]
        splitString' toSplit@(c:cs) strAccum
            | startsWith delimiter toSplit = strAccum : splitString' (drop (length delimiter) toSplit) ""
            | otherwise = splitString' cs (strAccum ++ [c])
    in
    splitString' toSplit ""

startsWith :: String -> String -> Bool
startsWith [] _ = True
startsWith _ [] = False
startsWith (c1 : rest1) (c2 : rest2)
    | c1 == c2 = startsWith rest1 rest2
    | otherwise = False


strToInt :: String -> Integer
strToInt [] = 0
strToInt string = read string :: Integer

getNum :: String -> String
getNum [] = []
getNum (c : cs)
    | isDigit c = c : getNum cs
    | otherwise = []
