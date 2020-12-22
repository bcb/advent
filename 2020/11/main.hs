import Data.Map as Map (Map, lookup, fromList, findWithDefault, toList)
import Data.Maybe

boardWidth = 95
boardHeight = 93

type Board = Map (Int, Int) Position
data Position = Floor | EmptySeat | OccupiedSeat deriving (Show, Eq)

positionFromChar :: Char -> Position
positionFromChar '.' = Floor
positionFromChar 'L' = EmptySeat
positionFromChar '#' = OccupiedSeat

charFromPosition :: Position -> Char
charFromPosition Floor = '.'
charFromPosition EmptySeat = 'L'
charFromPosition OccupiedSeat = '#'

populateBoard :: [String] -> Board
populateBoard input = Map.fromList [((x,y), positionFromChar c) | (y, row) <- zip [0..] input, (x, c) <- zip [0..] row]

visibleCoords :: (Int, Int) -> [[(Int, Int)]]
visibleCoords (x, y) = [
    zip [x-1, x-2..0] [y-1, y-2..0],
    zip (repeat x) [y-1, y-2..0],
    zip [x+1, x+2..boardWidth] [y-1, y-2..0],
    zip [x-1, x-2..0] (repeat y),
    zip [x+1, x+2..boardWidth] (repeat y),
    zip [x-1, x-2..0] [y+1, y+2..boardHeight],
    zip (repeat x) [y+1, y+2..boardHeight],
    zip [x+1, x+2..boardWidth] [y+1, y+2..boardHeight] ]

safeHead :: [Maybe a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = x

firstVisibleSeat :: [Maybe Position] -> Maybe Position
firstVisibleSeat = safeHead.filter (`elem` [Just EmptySeat, Just OccupiedSeat])

visibleSeats :: [[(Int, Int)]] -> Board -> [Maybe Position]
visibleSeats coords board =  fmap firstVisibleSeat $ fmap (fmap (\c -> Map.lookup c board)) coords

applyRule :: Position -> [Maybe Position] -> Position
applyRule Floor _ = Floor
applyRule EmptySeat adjacents
    | ((length.filter (==Just OccupiedSeat) $ adjacents) ==0) = OccupiedSeat
    | otherwise = EmptySeat
applyRule OccupiedSeat adjacents
    | ((length.filter (==Just OccupiedSeat) $ adjacents) >=5) = EmptySeat
    | otherwise = OccupiedSeat

posChange :: Board -> (Int, Int) -> Position
posChange board coords =
    case Map.lookup coords board of
        Just pos -> applyRule pos (visibleSeats (visibleCoords coords) board)
        Nothing -> Floor

step :: Board -> Board
step board = Map.fromList $ fmap (\c -> (c, posChange board c)) [(x, y) | x <- [0..boardWidth], y <- [0..boardHeight]]

stepUntilSettled :: Board -> Board
stepUntilSettled board =
    let result = step board
    in
    if result == board then
        result
    else
        stepUntilSettled result

boardAsText :: Board -> String
boardAsText board = unlines $ fmap (fmap (\c -> charFromPosition (Map.findWithDefault Floor c board))) [[(x, y) | x <- [0..9]] | y <- [0..9]]

main :: IO ()
main = do
    input <- readFile "input"
    print.length.filter (\(_, pos) -> pos == OccupiedSeat).Map.toList.stepUntilSettled.populateBoard.lines $ input
