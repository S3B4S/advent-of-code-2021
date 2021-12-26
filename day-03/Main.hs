import Debug.Trace
import Data.Char (digitToInt)
import Data.List (find)

data Count = Count [Int] Int deriving (Show) -- First is the count, second is the total entries

calculateGammaRateBinary :: Count -> String
calculateGammaRateBinary (Count count total) = map (\x -> let ones = total - x in if ones > total - ones then '1' else '0') count

-- Binary -> index -> decimal
binaryToDecimal :: Int -> String -> Int
binaryToDecimal (-1) bins = binaryToDecimal 0 (reverse bins)
binaryToDecimal n [] = 0
binaryToDecimal n (x : xs) = (2^n) * (read . pure) x + binaryToDecimal (n + 1) xs

-- "01011" -> "10100"
invertBinary :: String -> String
invertBinary [] = []
invertBinary ('1' : xs) = '0' : invertBinary xs
invertBinary ('0' : xs) = '1' : invertBinary xs

-- [3, 2, 1, 0, 4] -> "01100" -> [3, 3, 2, 0, 4]
countOnes :: [Int] -> String -> [Int]
countOnes [] bits = countOnes (take (length bits) $ repeat 0) bits
countOnes count bits = map (uncurry (+)) $ zip count $ map digitToInt bits

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)

-- Part 1
-- main :: IO ()
-- main = print
--   . uncurry (*)
--   . mapTuple (binaryToDecimal (-1))
--   . (\x -> (invertBinary x, x)) 
--   . calculateGammaRateBinary
--   . (\lines -> Count (foldl countOnes [] lines) (length lines))
--   . lines =<< readFile "input.txt"
-- Part 1 solution: 3885894

-- Part 2
invertBit :: Int -> Int
invertBit 1 = 0
invertBit 0 = 1
invertBit _ = -1

charToInt :: Char -> Int
charToInt '1' = 1
charToInt '0' = 0

data System = Oxygen | CO2

count :: Eq a => (a -> Bool) -> [a] -> Int
count f = length . filter f

determineBitCriteria :: System -> [(String, String)] -> Char
determineBitCriteria Oxygen bitSequences = if amountBitCriteria >= (length bitSequences - amountBitCriteria) then '1' else '0'
  where amountBitCriteria = count ((== '1') . head . fst) bitSequences
determineBitCriteria CO2 bitSequences = if amountBitCriteria <= (length bitSequences - amountBitCriteria) then '0' else '1'
  where amountBitCriteria = count ((== '0') . head . fst) bitSequences

filterBitCriteria :: System -> [(String, String)] -> [(String, String)]
filterBitCriteria system bitSequences = (map (\(f, s) -> (tail f, s)) . filter ((== bitCriteria) . head . fst)) bitSequences
  where bitCriteria = determineBitCriteria system bitSequences

main :: IO ()
main = print
  . (\(Just x, Just y) -> binaryToDecimal (-1) x * binaryToDecimal (-1) y)
  . (\x -> (
      (fmap (snd . head) . find ((== 1) . length) . iterate (filterBitCriteria Oxygen)) x,
      (fmap (snd . head) . find ((== 1) . length) . iterate (filterBitCriteria CO2)) x)
    )
  . map (\x -> (x, x))
  . lines =<< readFile "input.txt"
