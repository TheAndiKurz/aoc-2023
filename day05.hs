module Main where
import Util (splitString)

main :: IO ()
main = do
    contents <- readFile "day05.ex.data"
    
    let plantation = parsePlantation contents
    let locations = getEndLocations plantation
    print $ minimum locations
    
    let locations = getEndLocations2 plantation
    print $ minimum locations



type Map = [(Integer, Integer, Integer)]

data Plantation = 
    Plantation {
        seeds :: [Integer],
        soil :: Map,
        fertilizer :: Map,
        water :: Map,
        light :: Map,
        temperature :: Map,
        humidity :: Map,
        location :: Map
    }
    deriving Show

parsePlantation :: String -> Plantation
parsePlantation string =
    let 
        getMap mapStr = 
            let mapLines = drop 1 $ lines mapStr in
            let values line =
                    let (dstart : sstart : rlen : _) = splitString " " line in
                    (read dstart, read sstart, read rlen)
            in
            map values mapLines
    in
    let 
        (seedsStr : soilMapStr : fertilizerMapStr : waterMapStr : lightMapStr : temperatureMapStr : humidityMapStr : locationMapStr : _) = 
            splitString "\n\n" string 
    in
    let seeds = map read $ drop 1 $ splitString " " seedsStr in
    
    Plantation {
        seeds=seeds, 
        soil=getMap soilMapStr, 
        fertilizer=getMap fertilizerMapStr, 
        water=getMap waterMapStr, 
        light=getMap lightMapStr, 
        temperature=getMap temperatureMapStr,
        humidity=getMap humidityMapStr,
        location=getMap locationMapStr
    }

-- part 1
getEndLocations :: Plantation -> [Integer]
getEndLocations (Plantation seeds soil fertilizer water light temperature humidity location) =
    let 
        applyMap :: Map -> Integer -> Integer
        applyMap [] seed = seed
        applyMap ((dstart, sstart, rlen) : rest) seed
            | sstart <= seed && seed < (sstart + rlen) = dstart + seed - sstart
            | otherwise = applyMap rest seed
    in
    
    map (applyMap location) 
        $ map (applyMap humidity) 
        $ map (applyMap temperature) 
        $ map (applyMap light)
        $ map (applyMap water)
        $ map (applyMap fertilizer)
        $ map (applyMap soil)
        seeds

-- part 2
rangeFromLen :: Integer -> Integer -> [Integer]
rangeFromLen i1 i2 = [x | x <- [i1..(i1 + i2)]]

zipInPlace :: [Integer] -> [(Integer, Integer)]
zipInPlace [] = []
zipInPlace [a] = []
zipInPlace (i1 : i2 : rest) = (i1, i2) : zipInPlace rest



getEndLocations2 :: Plantation -> [Integer]
getEndLocations2 (Plantation seeds soil fertilizer water light temperature humidity location) =
    let
        applyMap :: Map -> Integer -> Integer
        applyMap [] seed = seed
        applyMap ((dstart, sstart, rlen) : rest) seed
            | sstart <= seed && seed < (sstart + rlen) = dstart + seed - sstart
            | otherwise = applyMap rest seed
    in

    map (applyMap location) 
        $ map (applyMap humidity) 
        $ map (applyMap temperature) 
        $ map (applyMap light)
        $ map (applyMap water)
        $ map (applyMap fertilizer)
        $ map (applyMap soil)
        $ concatMap (\(i1, i2) -> rangeFromLen i1 i2) $ zipInPlace seeds


