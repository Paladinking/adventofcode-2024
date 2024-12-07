import Data.List (elemIndex, sortBy)
import Data.Maybe (fromJust, isNothing, isJust)


main :: IO()
main = do
    input <- readFile "input/input5.txt"
    let content = lines input

    let empty = fromJust $ elemIndex "" content

    let (rules_str, input_tail) = splitAt empty content
    let pages_str = drop 1 input_tail
    let rules = map parseRule rules_str
    let updates = map parseUpdate pages_str

    let valid = filter (correcUpdate rules) updates
    let invalid = filter (not . correcUpdate rules) updates
    let fixed = map (sortBy (allowedOrder rules)) invalid

    let middle update = update !! (length update `div` 2)

    let score = sum $ map middle valid
    let fixed_score = sum $ map middle fixed

    print score
    print fixed_score


parseRule :: String -> (Int, Int)
parseRule s = (first, second) where
    sep = fromJust $ elemIndex '|' s
    first = read $ take sep s
    second = read $ drop (sep + 1) s


parseUpdate :: String -> [Int]
parseUpdate s | isNothing $ elemIndex ',' s = [read s]
parseUpdate s = parts where
    ix = fromJust $ elemIndex ',' s
    (a, b) = splitAt ix s
    parts = read a : parseUpdate (drop 1 b)


allowedOrder :: [(Int, Int)] -> Int -> Int -> Ordering
allowedOrder rules a b = if allowedBefore rules a b then LT else GT


allowedBefore :: [(Int, Int)] -> Int -> Int -> Bool
allowedBefore rules a b = not $ any (\(x, y) -> (y == a) && (x == b)) rules


allowedFirst :: [(Int, Int)] -> Int -> [Int] -> Bool
allowedFirst rules page after = null breaks where
    breaks = filter (\(x, y) -> (y == page) && isJust (elemIndex x after)) rules


correcUpdate :: [(Int, Int)] -> [Int] -> Bool
correcUpdate _ pages | length pages <= 1 = True
correcUpdate rules pages = allowed where
    first = head pages
    after = drop 1 pages
    allowed = allowedFirst rules first after && correcUpdate rules after

