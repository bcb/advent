import Data.List (sort)
import Data.List.Split (splitOn)

occurrences :: Eq a => [a] -> a -> Int
occurrences lst x = length . filter (x ==) $ lst

main :: IO ()
main = do
  lst <- readFile "input"
  let splitted = map (splitOn "   ") . lines $ lst
      fsts = map head $ splitted
      snds = map (!! 1) $ splitted
  print . sum . map (\x -> (read x :: Int) * occurrences snds x) $ fsts
