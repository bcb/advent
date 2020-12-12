--- Day 4: Passport Processing
import Data.Char
import Data.List
import Data.List.Split
import Data.Map as Map
import Data.Validation
import Text.Regex.PCRE

data Passport = Passport {
    byr :: String,
    iyr :: String,
    eyr :: String,
    hgt :: String,
    hcl :: String,
    ecl :: String,
    pid :: String
} deriving (Show)

newtype Error = Error [String] deriving Show

maybeBool :: String -> Bool -> String -> Validation Error String
maybeBool _ True s = Success s
maybeBool err False _ =  Failure (Error [err])

eqlen :: String -> Int -> String -> Validation Error String
eqlen err l s = maybeBool err (length s == l) s

minval :: String -> Int -> String -> Validation Error String
minval err l s = maybeBool err ((read s::Int) <= l) s

maxval :: String -> Int -> String -> Validation Error String
maxval err l s = maybeBool err ((read s ::Int) >= l) s

iselem :: String -> [String] -> String -> Validation Error String
iselem err lst s = maybeBool err (elem s lst) s

regex :: String -> String -> String -> Validation Error String
regex err pattern s = maybeBool err (s =~ pattern) s

validateByr :: String -> Validation Error String
validateByr s =
    case length s == 4 &&  minval "byr" 1920 <*> maxval "byr" 2002) of
        True -> Success s
        False -> Failure (Error [""])

validateHgt :: String -> Validation Error String
validateHgt s
    | ("cm" `isSuffixOf` s) = return (takeWhile isDigit s) >>= minval "hgt" 150 >>= maxval "hgt" 193
    | ("in" `isSuffixOf` s) = return (takeWhile isDigit s) >>= maxval "hgt" 59 >>= maxval "hgt" 76
    | otherwise = Failure (Error ["Doesn't end in cm or in"])

-- Convert a mapping into a Passport type, validating each credential
makePassport :: Map String String -> Maybe Passport
makePassport map =
    Passport
        <$> validateByr (Map.lookup "byr" map)
        <*> (Map.lookup "iyr" map >>= eqlen "iyr" 4 >>= minval "iyr" 2010 >>= maxval "iyr" 2020)
        <*> (Map.lookup "eyr" map >>= eqlen "eyr" 4 >>= minval "eyr" 2020 >>= maxval "eyr" 2030)
        <*> (Map.lookup "hgt" map >>= validateHgt)
        <*> (Map.lookup "hcl" map >>= regex "hcl" "^#[0-9a-f]{6}$")
        <*> (Map.lookup "ecl" map >>= iselem "ecl" ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
        <*> (Map.lookup "pid" map >>= eqlen "pid" 9)

-- Separate a credential into a tuple, e.g. "byr:2003" -> ("byr", "2003")
separate :: String -> (String, String)
separate str =
    let parts = break (==':') str
    in ((fst parts), (tail $ snd parts))

-- Parse a raw string into a mapping of each credential
parse :: String -> Map String String
parse raw = Map.fromList $ separate <$> (words $ raw)

main :: IO ()
main = do
    input <- readFile "input"
    -- length.(filter isJust)
    print $ makePassport.parse <$> splitOn "\n\n" input
