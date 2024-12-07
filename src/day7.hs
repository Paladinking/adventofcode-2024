

main :: IO()
main = do
    input <- readFile "input/input7.txt"
    let content = lines input

    let rows = map split content

    print $ sum $ map fst $ filter isValid rows
    print $ sum $ map fst $ filter isValid2 rows


countDigits :: Integer -> Integer
countDigits n | n < 10 = 1
countDigits n = 1 + countDigits (n `div` 10)


intCat :: Integer -> Integer -> Integer
intCat a b = (10 ^ countDigits b) * a + b


split :: String -> (Integer, [Integer])
split s = (res, vals) where
    parts = words s
    res_str = take (length (head parts) - 1) $ head parts
    res = read res_str
    vals = map read (drop 1 parts)


isValid :: (Integer, [Integer]) -> Bool
isValid (_, []) = False
isValid (res, n1:vals) = valid vals n1 where
    valid nums acc
      | null nums = acc == res
      | otherwise = valid (tail nums) (acc + head nums) ||
                    valid (tail nums) (acc * head nums)


isValid2 :: (Integer, [Integer]) -> Bool
isValid2 (_, []) = False
isValid2 (res, n1:vals) = valid vals n1 where
    valid nums acc
      | null nums = acc == res
      | otherwise = valid (tail nums) (acc + head nums) ||
                    valid (tail nums) (acc * head nums) ||
                    valid (tail nums) (acc `intCat` head nums)
