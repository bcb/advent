import Data.List (sort)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  input <- readFile "input"
  let elves = (map . map) (read :: String -> Int) . map words . splitOn "\n\n" $ input
  print . sum . take 3 . reverse . sort . map sum $ elves
