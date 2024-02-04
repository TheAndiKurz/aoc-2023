module Main where
import Data.List

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Show, Eq, Ord)

type Hand = [Card]

data Bid = Bid {
    hand :: Hand,
    amount :: Integer
} deriving (Show)

data HandClass = HighCard | OnePair | TwoPairs | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
    deriving (Show, Eq, Ord)

main :: IO ()
main = do
    content <- readFile "day07.data"
    let bids = map parseBid $ lines content
    print $ sum $ map (\(rank, Bid {hand, amount}) -> rank * amount) $ zip [1..] $ sort bids

parseBid :: String -> Bid
parseBid s = Bid { hand = parseCard $ take 5 s, amount = read $ drop 6 s }

parseCard :: String -> Hand
parseCard s = map parseCard' s

parseCard' :: Char -> Card
parseCard' 'A' = Ace
parseCard' 'K' = King
parseCard' 'Q' = Queen
parseCard' 'J' = Jack
parseCard' 'T' = Ten
parseCard' '9' = Nine
parseCard' '8' = Eight
parseCard' '7' = Seven
parseCard' '6' = Six
parseCard' '5' = Five
parseCard' '4' = Four
parseCard' '3' = Three
parseCard' '2' = Two
parseCard' c = error $ "Invalid card: " ++ [c]

isFourOfAKind :: Hand -> Bool
isFourOfAKind s = any (== 4) $ map (\x -> length $ filter (== x) s) s

isThreeOfAKind :: Hand -> Bool
isThreeOfAKind s = any (== 3) $ map (\x -> length $ filter (== x) s) s

handClass :: Hand -> HandClass
handClass [] = error "Empty hand"
handClass s =
    let distinct = nub s in

    case length distinct of
        1 -> FiveOfAKind
        2 -> if isFourOfAKind s then FourOfAKind else FullHouse
        3 -> if isThreeOfAKind s then ThreeOfAKind else TwoPairs
        4 -> OnePair
        5 -> HighCard


compareHand a b =
    let comp = compare (handClass a) (handClass b) in
    let 
        compare' [] [] = EQ
        compare' (a : as) (b : bs)
            | a == b = compare' as bs
            | otherwise = compare a b
    in
    if comp == EQ then compare' a b else comp

instance Eq Bid where
    a == b = compare a b == EQ

instance Ord Bid where
    compare a b = compareHand (hand a) (hand b)
