import Data.List
import Control.Arrow ((&&&))
import Data.Bifunctor (bimap)

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

amendCoordinatesWithAim :: Direction -> (Int, Int, Int) -> (Int, Int, Int)
amendCoordinatesWithAim (Forward n) (x, y, aim) = (x + n, y + aim * n, aim)
amendCoordinatesWithAim (Up n) (x, y, aim) = (x, y, aim - n)
amendCoordinatesWithAim (Down n) (x, y, aim) = (x, y, aim + n)

-- This involes an aim, the degree of the aim altered by up and down
calculateCoordinatesP2 :: [Direction] -> (Int, Int, Int)
calculateCoordinatesP2 = foldl (flip amendCoordinatesWithAim) (0, 0, 0) -- (X, Y, currentaim)

tripleToDouble :: (a, b, c) -> (a, b)
tripleToDouble (x, y, z) = (x, y)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)

main :: IO ()
main = print . mapTuple (uncurry (*)) . (calculateCoordinatesP1 &&& tripleToDouble . calculateCoordinatesP2) . map lineToDirection . lines =<< readFile "input.txt"
-- Part 1 solution: 1660158
-- Part 2 solution: 1604592846
