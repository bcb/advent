import Data.List (sort)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  lst <- readFile "input"
  let splitted = map (splitOn "   ") . lines $ lst
      fsts = sort . map (\x -> read x :: Int) . map head $ splitted
      snds = sort . map (\x -> read x :: Int) . map (!! 1) $ splitted
  print . sum . map (\(x, y) -> abs $ x - y) $ zip fsts snds
