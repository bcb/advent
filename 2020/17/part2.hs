import Data.Maybe (fromJust,isJust)

boardWidth = 8
boardHeight = 8

data Coord = Coord Int Int Int Int deriving (Show, Eq)

getX, getY, getZ :: Coord -> Int
getX (Coord x _ _ _) = x
getY (Coord _ y _ _) = y
getZ (Coord _ _ z _) = z
getW (Coord _ _ _ w) = w

countActiveNeighbours :: [Coord] -> [Coord] -> Int
countActiveNeighbours actives neighbourCoords =
    length.filter (==True).map (`elem` actives) $ neighbourCoords

getNeighbourCoords :: Coord -> [Coord]
getNeighbourCoords coord@(Coord x y z w) =
    filter (/=coord) $ [Coord i j k l | i <- [x-1..x+1], j <- [y-1..y+1], k <- [z-1..z+1], l <- [w-1..w+1]]

applyCube :: [Coord] -> Coord -> Maybe Coord
applyCube actives coord
    | (coord `elem` actives) = if activeNeighbours `elem` [2,3] then Just coord else Nothing
    | otherwise = if activeNeighbours == 3 then Just coord else Nothing
    where activeNeighbours = countActiveNeighbours actives $ getNeighbourCoords coord

step :: [Coord] -> Int -> [Coord]
step actives _ =
    map fromJust.filter isJust $ [applyCube actives (Coord x y z w) |
        x <- [(minimum.map getX $ actives)-1..(maximum.map getX $ actives)+1],
        y <- [(minimum.map getY $ actives)-1..(maximum.map getY $ actives)+1],
        z <- [(minimum.map getZ $ actives)-1..(maximum.map getZ $ actives)+1],
        w <- [(minimum.map getW $ actives)-1..(maximum.map getW $ actives)+1]]

main :: IO ()
main = do
    input <- concat.lines <$> getContents
    let actives = map fst.filter (\(_, c) -> c == '#').zip [Coord x y 0 0 | x <- [0..boardWidth-1], y <- [0..boardHeight-1]] $ input
    print.length $ foldl step actives [0..5]
