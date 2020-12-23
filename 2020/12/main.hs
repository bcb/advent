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

turnleft :: Facing -> Facing
turnleft FacingNorth = FacingWest
turnleft FacingWest = FacingSouth
turnleft FacingSouth = FacingEast
turnleft FacingEast = FacingNorth

turnright :: Facing -> Facing
turnright FacingNorth = FacingEast
turnright FacingEast = FacingSouth
turnright FacingSouth = FacingWest
turnright FacingWest = FacingNorth

moveForward :: Facing -> Position -> Int -> Position
moveForward FacingNorth (Position e n) x = Position e (n+x)
moveForward FacingEast (Position e n) x = Position (e+x) n
moveForward FacingSouth (Position e n) x = Position e (n-x)
moveForward FacingWest (Position e n) x = Position (e-x) n

move :: State -> Move -> State
move (State (Position e n) f) (MoveNorth x) = State (Position e (n+x)) f
move (State (Position e n) f) (MoveSouth x) = State (Position e (n-x)) f
move (State (Position e n) f) (MoveEast x) = State (Position (e+x) n) f
move (State (Position e n) f) (MoveWest x) = State (Position (e-x) n) f
move (State (Position e n) f) (TurnLeft x) = State (Position e n) (iterate turnleft f !! (x `div` 90))
move (State (Position e n) f) (TurnRight x) = State (Position e n) (iterate turnright f !! (x `div` 90))
move (State p f) (MoveForward x) = State (moveForward f p x) f

parse :: String -> Move
parse ('N':xs) = MoveNorth $ read xs
parse ('S':xs) = MoveSouth $ read xs
parse ('E':xs) = MoveEast $ read xs
parse ('W':xs) = MoveWest $ read xs
parse ('L':xs) = TurnLeft $ read xs
parse ('R':xs) = TurnRight $ read xs
parse ('F':xs) = MoveForward $ read xs

manhattan :: State -> Int
manhattan (State (Position e n) _) = abs e + abs n

main :: IO ()
main = do
    input <- readFile "input"
    print.manhattan.foldl' move (State (Position 0 0) FacingEast).fmap parse.lines $ input
