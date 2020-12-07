--- Day 4: Passport Processing
import Data.List.Split

data Cred = Cred { key :: String, val :: String } deriving (Show)

valid :: Cred -> Bool
valid cred = elem (key cred) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

parse :: String -> Cred
parse str =
    let parts = break (==':') str
    in Cred (fst parts) (tail $ snd parts)

main :: IO ()
main = do
    input <- readFile "input"
    let parsed = fmap (fmap parse).fmap words $ splitOn "\n\n" input
    print $ length.filter (==7) $ fmap (length.filter valid) parsed
