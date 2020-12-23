import Data.List (foldl')

data ShipPosition = ShipPosition Int Int deriving Show
data WaypointPosition = WaypointPosition Int Int deriving Show
data Move = MoveNorth Int | MoveSouth Int | MoveEast Int | MoveWest Int | RotateLeft Int | RotateRight Int | MoveForward Int deriving (Eq, Show)

rotateLeft :: ShipPosition -> WaypointPosition -> Int -> WaypointPosition
rotateLeft (ShipPosition se sn) (WaypointPosition we wn) x =
    case x `mod` 360 of
        90 -> WaypointPosition (-wn) we
        180 -> WaypointPosition (-we) (-wn)
        270 -> WaypointPosition wn (-we)

rotateRight s w x = rotateLeft s w (-x)

moveForward :: ShipPosition -> WaypointPosition -> Int -> ShipPosition
moveForward (ShipPosition se sn) (WaypointPosition we wn) x = ShipPosition (se+we*x) (sn+wn*x)

move :: (ShipPosition, WaypointPosition) -> Move -> (ShipPosition, WaypointPosition)
move (s, WaypointPosition e n) (MoveNorth x) = (s, WaypointPosition e (n+x))
move (s, WaypointPosition e n) (MoveSouth x) = (s, WaypointPosition e (n-x))
move (s, WaypointPosition e n) (MoveEast x) = (s, WaypointPosition (e+x) n)
move (s, WaypointPosition e n) (MoveWest x) = (s, WaypointPosition (e-x) n)
move (s, w) (RotateLeft x) = (s, rotateLeft s w x)
move (s, w) (RotateRight x) = (s, rotateRight s w x)
move st@(s, w) (MoveForward x) = (moveForward s w x, w)

parse :: String -> Move
parse ('N':xs) = MoveNorth $ read xs
parse ('S':xs) = MoveSouth $ read xs
parse ('E':xs) = MoveEast $ read xs
parse ('W':xs) = MoveWest $ read xs
parse ('L':xs) = RotateLeft $ read xs
parse ('R':xs) = RotateRight $ read xs
parse ('F':xs) = MoveForward $ read xs

manhattan :: ShipPosition -> Int
manhattan (ShipPosition e n) = abs e + abs n

main :: IO ()
main = do
    input <- readFile "input"
    print.manhattan.fst.foldl' move (ShipPosition 0 0, WaypointPosition 10 1).fmap parse.lines $ input
