import Data.List (sort)
import Data.List.Split (splitOn)

occurrences :: [Int] -> Int -> Int
occurrences lst x = length . filter (x ==) $ lst

main :: IO ()
main = do
  lst <- readFile "input"
  let splitted = map (splitOn "   ") . lines $ lst
      fsts = map (\x -> read x :: Int) . map head $ splitted
      snds = map (\x -> read x :: Int) . map (!! 1) $ splitted
  print . sum . map (\x -> x * occurrences snds x) $ fsts
