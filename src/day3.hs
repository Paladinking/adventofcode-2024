import Data.List (elemIndices, isPrefixOf)
import Text.Read (readMaybe)

main :: IO ()
main = do
    input <- readFile "input/input3.txt"

    print $ result input
    print $ toggleResult input True


result :: String -> Int
result s | null s = 0
result s = prod + result (drop chars s) where
    (prod, chars) = readMul s


toggleResult :: String -> Bool -> Int
toggleResult s _ | null s = 0
toggleResult s b = val where
    (prod, chars) = readMul s
    val
      | "do()" `isPrefixOf` s = toggleResult (drop 4 s) True
      | "don't()" `isPrefixOf` s = toggleResult (drop 7 s) False
      | not b = toggleResult (drop 1 s) False
      | otherwise = prod + toggleResult (drop chars s) True

    
readMul :: String -> (Int, Int)
readMul s | not ("mul(" `isPrefixOf` s) = (0, 1)
readMul s | null $ elemIndices ',' s = (0, 4)
readMul s | null $ elemIndices ')' s = (0, 4)
readMul s = val where
    sep = head $ elemIndices ',' s
    end = head $ elemIndices ')' s
    first = readMaybe $ drop 4 (take sep s)
    second = readMaybe $ drop (sep + 1) (take end s)
    val = case (first, second) of
        (Just a, Just b) -> (a * b, end + 1)
        _ -> (0, 4)
