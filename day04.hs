module Main where
import Debug.Trace (trace)
import Util (splitStr)

main :: IO ()
main = do
    contents <- readFile "day04.data"
    
    let cards = map parseCard $ lines contents
    let cardScores = map cardScore cards
    print $ sum cardScores

    print $ copiesFast cards


data Card = 
    Card {
        cid :: Integer,
        winning :: [Integer],
        cards :: [Integer]
    }
    deriving Show

instance Eq Card where 
    (Card cid1 _ _) == (Card cid2 _ _) = cid1 == cid2


parseCard :: String -> Card
parseCard cardStr =
    let (cidStr : contentStr : _) = splitStr ':' cardStr in
    let cid = read $ last $ splitStr ' ' cidStr in

    let (winningStr : cardsStr : _) = splitStr '|' contentStr in
    let numArr str = map read $ filter (\s -> length s > 0) $ splitStr ' ' str in
    let 
        winning = numArr winningStr
        cards = numArr cardsStr
    in
    
    Card {cid=cid, winning=winning, cards=cards}

numWinningCard :: Card -> Integer
numWinningCard (Card _ winning cards) =
    let 
        numWinningNumbers winning [] = 0
        numWinningNumbers winning (num : rest)
            | num `elem` winning = 1 + numWinningNumbers winning rest
            | otherwise = numWinningNumbers winning rest
    in
    numWinningNumbers winning cards


-- part 1
cardScore :: Card -> Integer
cardScore card =
    let numWinning = numWinningCard card in
    if numWinning == 0 then 0
    else 2 ^ (numWinning - 1)

-- part 2
copies :: [Card] -> Integer
copies cards =
    let 
        copies_restrict _ 0 = 0
        copies_restrict [] _ = 0
        copies_restrict (card : cards) count = 
            let numWinning = numWinningCard card in
            1 + copies_restrict cards (count - 1) + copies_restrict cards numWinning
    in
    copies_restrict cards $ toInteger $ length cards

-- part 2 much faster
copiesFast :: [Card] -> Integer
copiesFast cards =
    let rCards = reverse cards in
    let 
        lookupInt :: [(Integer, Integer)] -> Integer -> Integer
        lookupInt table cid = case lookup cid table of 
                                Just n -> n
                                Nothing -> 0
    in
    
    let 
        lookupRange :: [(Integer, Integer)] -> Integer -> Integer -> [Integer]
        lookupRange table cid 0 = []
        lookupRange table cid range = lookupInt table cid : lookupRange table (cid + 1) (range - 1)
    in

    let 
        constructLTable :: [Card] -> [(Integer, Integer)] -> [(Integer, Integer)]
        constructLTable [] lTable = lTable
        constructLTable (card@(Card cid _ _) : rest) lTable = 
            let numWinning = numWinningCard card in
            let cardCopies = lookupRange lTable (cid + 1) numWinning in
            constructLTable rest ((cid, 1 + sum cardCopies) : lTable)
    in
    sum $ map snd $ constructLTable rCards []

