module Main where
import Data.Char (isDigit, isSpace)
import Data.Text (splitOn)
import Debug.Trace (trace)
import Util (splitStr, getNum)

data Game = 
    Game {
    gid :: Integer,
    rounds :: [(Integer, Integer, Integer)]
    } deriving (Show)    


extractGame :: String -> Game
extractGame line@(c : cs) = Game {gid=getGameId (head splitColon), rounds=getGameRounds (last splitColon)}
    where splitColon = splitStr ':' line

getGameId :: String -> Integer
getGameId line@(c : cs)
    | isDigit c = read (getNum line)
    | otherwise = getGameId cs

getGameRounds :: String -> [(Integer, Integer, Integer)]
getGameRounds str = map getRound (splitStr ';' str)

getRound :: String -> (Integer, Integer, Integer)
getRound str = getRound' str (0, 0, 0) 0
    where
        getRound' [] v _ = v
        getRound' ('r' : 'e' : 'd' : cs) (r, g, b) currentNum = getRound' cs (currentNum, g, b) 0
        getRound' ('g' : 'r' : 'e' : 'e' : 'n' : cs) (r, g, b) currentNum = getRound' cs (r, currentNum, b) 0
        getRound' ('b' : 'l' : 'u' : 'e' : cs) (r, g, b) currentNum = getRound' cs (r, g, currentNum) 0
        getRound' str@(c : cs) v currentNum
            | isDigit c && currentNum == 0 = getRound' cs v (read (getNum str))
            | otherwise = getRound' cs v currentNum


isGamePossible :: Game -> (Integer, Integer, Integer) -> Bool
isGamePossible (Game _ rounds) = areRoundsPossible rounds
    where 
        areRoundsPossible [] _ = True
        areRoundsPossible ((gr, gg, gb) : rs) cubes@(r, g, b)
            | gr > r = False
            | gg > g = False
            | gb > b = False
            | otherwise = areRoundsPossible rs cubes

possibleGame :: Game -> (Integer, Integer, Integer) -> Integer
possibleGame g@(Game id _) c = if isGamePossible g c then id else 0


minBag :: Game -> (Integer, Integer, Integer)
minBag g@(Game _ rounds) = minBag' rounds (0, 0, 0)
    where 
    minBag' [] cubes = cubes
    minBag' ro@((gr, gg, gb) : gs) (r, g, b)
        | gr > r = minBag' ro (gr, g, b)
        | gg > g = minBag' ro (r, gg, b)
        | gb > b = minBag' ro (r, g, gb)
        | otherwise = minBag' gs (r, g, b)

gamePower :: Game -> Integer
gamePower game = r * g * b
    where (r, g, b) = minBag game

main :: IO ()
main = do
    contents <- readFile "day02.data"
    let games = map extractGame (lines contents)

    let possibleGames = map (\g -> possibleGame g (12, 13, 14)) games
    print (sum possibleGames)
    
    let gamePowers = map gamePower games
    print (sum gamePowers)

