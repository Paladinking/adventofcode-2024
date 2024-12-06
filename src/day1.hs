import Data.List (sort)
import Data.Foldable (foldl')


main :: IO()
main = do
    input <- readFile "input/input1.txt"
    let content = lines input

    let pairs = map split content
    let first_num = sort $ map first pairs where
            first (x, _) = x
    let second_num = sort $ map second pairs where
            second (_, y) = y

    let total = sum $ zipWith (\x y -> abs $ x - y) first_num second_num
   
    let score = foldl' (\n x -> n + x * count_char second_num x) 0 first_num
        count_char list c = length . filter (== c) $ list

    print total
    print score


split :: String -> (Int, Int)
split line = (first, second) where
    parts = words line
    first = read $ head parts
    second = read $ parts !! 1
