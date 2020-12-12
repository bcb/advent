--- Day 4: Passport Processing
import Data.Char
import Data.List
import Data.List.Split
import Data.Map as Map
import Data.Validation
import Text.Regex.PCRE

newtype Error = Error [String] deriving Show

instance Semigroup Error where
    Error xs <> Error ys = Error (xs ++ ys)

data Passport = Passport {
    byr :: String,
    iyr :: String,
    eyr :: String,
    hgt :: String,
    hcl :: String,
    ecl :: String,
    pid :: String
} deriving (Show)

eqlen :: Int -> String -> Validation Error String
eqlen l s =
    case (length s == l) of
        True -> Success s
        False -> Failure (Error ["Incorrect length"])

minval :: Int -> String -> Validation Error String
minval l s =
    case ((read s::Int) >= l) of
        True -> Success s
        False -> Failure (Error ["Below min val"])

maxval :: Int -> String -> Validation Error String
maxval l s =
    case ((read s ::Int) <= l) of
        True -> Success s
        False -> Failure (Error ["Above max val"])

iselem :: [String] -> String -> Validation Error String
iselem lst s =
    case (elem s lst) of
        True -> Success s
        False -> Failure (Error ["Not a valid value"])

regex :: String -> String -> Validation Error String
regex pattern s =
    case (s =~ pattern) of
        True -> Success s
        False -> Failure (Error ["Does not match pattern"])


validateByr :: Maybe String -> Validation Error String
validateByr Nothing = Failure (Error ["byr missing"])
validateByr (Just s) = eqlen 4 s *> minval 1920 s *> maxval 2002 s

validateIyr :: Maybe String -> Validation Error String
validateIyr Nothing = Failure (Error ["iyr missing"])
validateIyr (Just s) = eqlen 4 s *> minval 2010 s *> maxval 2020 s

validateEyr :: Maybe String -> Validation Error String
validateEyr Nothing = Failure (Error ["eyr missing"])
validateEyr (Just s) = eqlen 4 s *> minval 2020 s *> maxval 2030 s

validateHgt :: Maybe String -> Validation Error String
validateHgt Nothing = Failure (Error ["hgt missing"])
validateHgt (Just s)
    | ("cm" `isSuffixOf` s) = minval 150 digits *> maxval 193 digits
    | ("in" `isSuffixOf` s) = minval 59 digits *> maxval 76 digits
    | otherwise = Failure (Error ["Doesn't end in cm or in"])
    where digits = takeWhile isDigit s

validateHcl :: Maybe String -> Validation Error String
validateHcl Nothing = Failure (Error ["hcl missing"])
validateHcl (Just s) = regex "^#[0-9a-f]{6}$" s

validateEcl :: Maybe String -> Validation Error String
validateEcl Nothing = Failure (Error ["ecl missing"])
validateEcl (Just s) = iselem ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"] s

validatePid :: Maybe String -> Validation Error String
validatePid Nothing = Failure (Error ["pid missing"])
validatePid (Just s) = regex "^[0-9]{9}$" s

-- Convert a mapping into a Passport type, validating each credential
makePassport :: Map String String -> Validation Error Passport
makePassport map =
    Passport
        <$> validateByr (Map.lookup "byr" map)
        <*> validateIyr (Map.lookup "iyr" map)
        <*> validateEyr (Map.lookup "eyr" map)
        <*> validateHgt (Map.lookup "hgt" map)
        <*> validateHcl (Map.lookup "hcl" map)
        <*> validateEcl (Map.lookup "ecl" map)
        <*> validatePid (Map.lookup "pid" map)

-- Separate a credential into a tuple, e.g. "byr:2003" -> ("byr", "2003")
separate :: String -> (String, String)
separate str =
    let parts = break (==':') str
    in ((fst parts), (tail $ snd parts))

-- Parse a raw string into a mapping of each credential
parse :: String -> Map String String
parse raw = Map.fromList $ separate <$> (words $ raw)

isSuccess :: Validation a b -> Bool
isSuccess (Failure _) = False
isSuccess _ = True

main :: IO ()
main = do
    input <- readFile "input"
    print.length.Data.List.filter isSuccess.fmap (makePassport.parse) $ splitOn "\n\n" input
