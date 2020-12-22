import Data.List (foldl')

data Position = Position Int Int deriving (Eq, Show)
data Facing = FacingNorth | FacingEast | FacingSouth | FacingWest deriving (Eq, Show)
data State = State Position Facing deriving Show
data Move =
    MoveNorth Int
    | MoveSouth Int
    | MoveEast Int
    | MoveWest Int
    | TurnLeft Int
    | TurnRight Int
    | MoveForward Int
    deriving (Eq, Show)

move :: State -> Move -> State
move (State (Position e n) f) (MoveNorth x) = State (Position e (n+x)) f
move (State (Position e n) f) (MoveSouth x) = State (Position e (n-x)) f
move (State (Position e n) f) (MoveEast x) = State (Position (e+x) n) f
move (State (Position e n) f) (MoveWest x) = State (Position (e-x) n) f
move (State (Position e n) f) (TurnLeft x) = State (Position e n) 

parse :: String -> Move
parse ('N':xs) = MoveNorth $ read xs
parse ('S':xs) = MoveSouth $ read xs
parse ('E':xs) = MoveEast $ read xs
parse ('W':xs) = MoveWest $ read xs
parse ('L':xs) = TurnLeft $ read xs
parse ('R':xs) = TurnRight $ read xs
parse ('F':xs) = MoveForward $ read xs

main :: IO ()
main = do
    input <- readFile "input"
    print.foldl' move (State (Position 0 0) FacingEast).fmap parse.lines $ input
