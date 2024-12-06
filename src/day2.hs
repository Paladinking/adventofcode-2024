

main :: IO()
main = do
    input <- readFile "input/input2.txt"
    let content = lines input

    let reports = map split content
    let safe_reports = filter safe reports
    let kinda_safe_reports = filter kindaSafe reports
    print . length $ safe_reports
    print . length $ kinda_safe_reports



split :: String -> [Int]
split line = map read $ words line


remove :: Int -> [a] -> [a]
remove ix list = joined where
    (p1, p2) = splitAt ix list
    joined = p1 ++ tail p2


kindaSafe :: [Int] -> Bool
kindaSafe report = safe report || any safe reports where
    reports = map (`remove` report) [0..length report - 1]


safe :: [Int] -> Bool
safe report = safePair $ zip report (drop 1 report)

safePair :: [(Int, Int)] -> Bool
safePair report = all increasing report || all decreasing report where
    close (a, b) = abs (a - b) <= 3
    increasing (a, b) = (b > a) && close(a, b)
    decreasing (a, b) = (b < a) && close(a, b)
