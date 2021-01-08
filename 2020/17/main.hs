newtype Coord = Coord (Int, Int, Int)

countActiveNeighbours :: [Coord] -> [Coord] -> Int
countActiveNeighbours neighbourCoords actives =
    length.filter (==True).map (elem actives) $ neighbourCoords

getNeighbourCoords :: Coord -> [Coord]
getNeighbourCoords coord@(x,y,z) =
    filter (==coord).[(i,j,k) | i <- [x-1..x+1], j <- [y-1..y+1], k <- [z-1..z+1]]

applyCube :: [Coord] -> Coord -> [Coord]
applyCube actives coord =
    let activeNeighbours = countActiveNeighbours (getNeighbourCoords coord) coord
    in 

step :: [Coord] -> Int -> [Coord]
step actives =
    concatMap (applyCube actives) actives

main :: IO ()
main = do
    input <- concat.lines <$> getContents
    let actives = map fst.filter (\(_, c) -> c == '#').zip [(x,y,0) | x <- [0..2], y <- [0..2]] $ input
    print $ foldl step actives [0..1]
