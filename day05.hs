module Main where
import Util (splitString)
import Debug.Trace

main :: IO ()
main = do
    contents <- readFile "day05.data"
    
    let plantation = parsePlantation contents
    let locations = getEndLocations plantation
    print $ minimum locations
    
    let locations = getEndLocations2 plantation
    print $ minimum $ map fst locations

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
zipInPlace :: [Integer] -> [(Integer, Integer)]
zipInPlace [] = []
zipInPlace [a] = []
zipInPlace (i1 : i2 : rest) = (i1, i2) : zipInPlace rest

getEndLocations2 :: Plantation -> [(Integer, Integer)]
getEndLocations2 (Plantation seeds soil fertilizer water light temperature humidity location) =
    let
        applyMap :: Map -> (Integer, Integer) -> [(Integer, Integer)]
        applyMap [] seed = [seed]
        applyMap ((dstart, sstart, rlen) : rest) (seedStart, seedEnd)
            | seedEnd < sstart = applyMap rest (seedStart, seedEnd)
            | seedStart >= (sstart + rlen) = applyMap rest (seedStart, seedEnd)
            | seedStart >= sstart && seedEnd < (sstart + rlen) = [(dstart + seedStart - sstart, dstart + seedEnd - sstart)]
            | seedStart >= sstart = 
                    (dstart + seedStart - sstart, dstart + rlen) : 
                    applyMap rest (sstart + rlen, seedEnd)
            | seedEnd < (sstart + rlen) = 
                    (dstart, dstart + seedEnd - sstart) : 
                    applyMap rest (seedStart, sstart - 1)
            | seedStart < sstart && seedEnd >= (sstart + rlen) = 
                    (dstart, dstart + rlen) : 
                    (applyMap rest (seedStart, sstart - 1) ++ applyMap rest (sstart + rlen, seedEnd))
            | otherwise = error ("should not happen " ++ show (dstart, sstart, rlen) ++ " " ++ show (seedStart, seedEnd))
    in

    concatMap (applyMap location) 
        $ concatMap (applyMap humidity) 
        $ concatMap (applyMap temperature) 
        $ concatMap (applyMap light)
        $ concatMap (applyMap water)
        $ concatMap (applyMap fertilizer)
        $ concatMap (applyMap soil)
        $ map (\(i1, i2) -> (i1, i1 + i2)) $ zipInPlace seeds


