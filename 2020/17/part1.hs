import Data.Maybe (fromJust,isJust)

boardWidth = 7 -- Zero based
boardHeight = 7 -- Zero based

newtype Coord = Coord (Int, Int, Int) deriving (Show, Eq)

fst3,snd3,trd3 :: Coord -> Int
fst3 (Coord (x,_,_)) = x
snd3 (Coord (_,x,_)) = x
trd3 (Coord (_,_,x)) = x

countActiveNeighbours :: [Coord] -> [Coord] -> Int
countActiveNeighbours actives neighbourCoords =
    length.filter (==True).map (`elem` actives) $ neighbourCoords

getNeighbourCoords :: Coord -> [Coord]
getNeighbourCoords coord@(Coord (x,y,z)) =
    filter (/=coord) $ [Coord (i,j,k) | i <- [x-1..x+1], j <- [y-1..y+1], k <- [z-1..z+1]]

applyCube :: [Coord] -> Coord -> Maybe Coord
applyCube actives coord
    | (coord `elem` actives) = if activeNeighbours `elem` [2,3] then Just coord else Nothing
    | otherwise = if activeNeighbours == 3 then Just coord else Nothing
    where activeNeighbours = countActiveNeighbours actives $ getNeighbourCoords coord

step :: [Coord] -> Int -> [Coord]
step actives _ =
    map fromJust.filter isJust $ [applyCube actives (Coord (x,y,z)) |
        x <- [(minimum.map fst3 $ actives)-1..(maximum.map fst3 $ actives)+1],
        y <- [(minimum.map snd3 $ actives)-1..(maximum.map snd3 $ actives)+1],
        z <- [(minimum.map trd3 $ actives)-1..(maximum.map trd3 $ actives)+1]]

main :: IO ()
main = do
    input <- concat.lines <$> getContents
    let actives = map fst.filter (\(_, c) -> c == '#').zip [Coord (x,y,0) | x <- [0..boardWidth], y <- [0..boardHeight]] $ input
    print.length $ foldl step actives [0..5]
