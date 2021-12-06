import Data.List

data Direction = Up Int | Down Int | Forward Int

lineToDirection :: String -> Direction
lineToDirection str
  | "forward" `isPrefixOf` str = Forward number
  | "up" `isPrefixOf` str = Up number
  | "down" `isPrefixOf` str = Down number
  | otherwise = error "unknown direction"
  where 
    number = read [last str]

amendCoordinates :: Direction -> (Int, Int) -> (Int, Int)
amendCoordinates (Forward n) (x, y) = (x + n, y)
amendCoordinates (Up n) (x, y) = (x, y - n)
amendCoordinates (Down n) (x, y) = (x, y + n)

-- This involves 
calculateCoordinatesP1 :: [Direction] -> (Int, Int)
calculateCoordinatesP1 = foldl (flip amendCoordinates) (0, 0)

-- This involes an aim, the degree of the aim altered by up and down
calculateCoordinatesP2 :: [Direction] -> (Int, Int)

main :: IO ()
main = print . uncurry (*) . calculateCoordinatesP1 . map lineToDirection . lines =<< readFile "input.txt"
-- Part 1 solution: 1660158
