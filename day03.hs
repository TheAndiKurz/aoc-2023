module Main where
import Data.Char (isDigit)
import Text.Printf (printf)
import Debug.Trace (trace)

isSymbol :: Char -> Bool
isSymbol c = c `elem` "&%*+-=@#$/"

emptyLine :: String
emptyLine = '.' : emptyLine

indexNums :: String -> [(Int, Int)]
indexNums [] = []
indexNums line = 
    let 
        startEndIndexs line index inDigit = case line of
            [] -> []
            (x : xs) ->
                if isDigit x && not inDigit
                    then index : startEndIndexs xs (index + 1) True
                else if not (isDigit x) && inDigit
                    then index : startEndIndexs xs (index + 1) False
                else if isDigit x
                    then startEndIndexs xs (index + 1) True
                else startEndIndexs xs (index + 1) False

        zipStartEnds [] = []
        zipStartEnds [x] = []
        zipStartEnds (x : y : xs) = (x, y) : zipStartEnds xs
    in
    let startEnds = startEndIndexs line 0 False in
    zipStartEnds startEnds    


prepLine :: String -> String
prepLine line = '.' : line ++ "."

slice :: String -> Int -> Int -> String
slice line start end = take (end - start) $ drop start line


squaresLine :: String -> String -> String -> [(Int, Int)] -> [(String, String, String)]
squaresLine _ _ _ [] = []

squaresLine top mid bot ((start, end) : rest) =
    let start' = start - 1
        end' = end + 1
    in
    let strNumTop = slice top start' end'
        strNumMid = slice mid start' end'
        strNumBot = slice bot start' end'
    in
    (strNumTop, strNumMid, strNumBot) : squaresLine top mid bot rest

numFromSquare :: (String, String, String) -> Integer
numFromSquare (top, mid, bot) = let foundSymbol = any isSymbol top || any isSymbol bot || any isSymbol mid
                                in
                                if foundSymbol
                                    then read $ slice mid 1 $ length mid - 1 :: Integer
                                    else 0

gearPositionsLine :: String -> [Int]
gearPositionsLine line = 
    let 
        gearPositionsLine' :: String -> Int -> [Int]
        gearPositionsLine' [] _ = []
        gearPositionsLine' (x : xs) index =
            if x == '*'
                then index : gearPositionsLine' xs (index + 1)
            else gearPositionsLine' xs (index + 1)
    in 
    gearPositionsLine' line 0

gearRatiosLine :: String -> String -> String -> [Int] -> [(String, String)]
gearRatiosLine _ _ _ [] = []
gearRatiosLine top mid bot positions =
    let
        partsPosTop = indexNums top
        partsPosMid = indexNums mid
        partsPosBot = indexNums bot
    in
    let 
        gearRatio :: [Int] -> [(String, String)]
        gearRatio [] = []
        gearRatio (pos : rest) =
            let adj = filter (\(start, end) -> (start - 1) <= pos && pos <= end) in
            let topAdj = adj partsPosTop in
            let midAdj = adj partsPosMid in
            let botAdj = adj partsPosBot in

            let numAdj = length topAdj + length midAdj + length botAdj in
            if numAdj /= 2
                then gearRatio rest
            else
                let topNums = map (uncurry (slice top)) topAdj in
                let midNums = map (uncurry (slice mid)) midAdj in
                let botNums = map (uncurry (slice bot)) botAdj in
                let nums = topNums ++ midNums ++ botNums in
                (head nums, last nums) : gearRatio rest
    in
    gearRatio positions

getGearRatio :: (String, String) -> Integer
getGearRatio (num1, num2) = read num1 * read num2


main :: IO ()
main = do
    contents <- readFile "day03.data"
    let rows = emptyLine : lines contents ++ [emptyLine]
    let lines = map prepLine rows
    let squares = concatMap (\(top, mid, bot) -> squaresLine top mid bot $ indexNums mid) (zip3 lines (tail lines) (tail $ tail lines))
    let nums = map numFromSquare squares
    printf "Part 1: %d\n" $ sum nums 

    let gearRatioNums = concatMap (\(top, mid, bot) -> gearRatiosLine top mid bot $ gearPositionsLine mid) (zip3 lines (tail lines) (tail $ tail lines))
    let gearRatios = map getGearRatio gearRatioNums
    printf "Part 2: %d\n" $ sum gearRatios 





