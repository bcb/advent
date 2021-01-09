import Data.Maybe (fromJust,isJust)

boardWidth = 7 -- Zero based
boardHeight = 7 -- Zero based

newtype Coord = Coord (Int, Int, Int) deriving (Show, Eq)

getX, getY, getZ :: Coord -> Int
getX (Coord (x,_,_)) = x
getY (Coord (_,y,_)) = y
getZ (Coord (_,_,z)) = z

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
        x <- [(minimum.map getX $ actives)-1..(maximum.map getX $ actives)+1],
        y <- [(minimum.map getY $ actives)-1..(maximum.map getY $ actives)+1],
        z <- [(minimum.map getZ $ actives)-1..(maximum.map getZ $ actives)+1]]

main :: IO ()
main = do
    input <- concat.lines <$> getContents
    let actives = map fst.filter (\(_, c) -> c == '#').zip [Coord (x,y,0) | x <- [0..boardWidth], y <- [0..boardHeight]] $ input
    print.length $ foldl step actives [0..5]
