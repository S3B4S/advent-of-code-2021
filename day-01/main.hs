import System.IO
import Control.Arrow

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow n list
  | n > length list = []
  | otherwise = take n list : slidingWindow n (drop 1 list)

boolToNumber :: Bool -> Int
boolToNumber True = 1
boolToNumber False = 0

countIncrements :: (a -> a -> Bool) -> [a] -> Int
countIncrements p list = foldr (\(x, y) c -> boolToNumber (p x y) + c) 0 $ zip list (tail list)

solvePuzzleP2 :: [Int] -> Int
solvePuzzleP2 depths = countIncrements (\x y -> sum y > sum x) xs
  where
    xs = slidingWindow 3 depths

solvePuzzleP1 :: [Int] -> Int
solvePuzzleP1 = countIncrements (flip (>))

main :: IO ()
main = print . (solvePuzzleP1 &&& solvePuzzleP2) . fmap read . lines =<< readFile "input.txt"
-- (1451, 1395)
