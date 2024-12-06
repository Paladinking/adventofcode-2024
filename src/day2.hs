

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

pairs :: [Int] -> [(Int, Int)]
pairs report = zip report (drop 1 report)

remove :: Int -> [a] -> [a]
remove ix list = joined where
    (p1, p2) = splitAt ix list
    joined = p1 ++ tail p2

kindaSafe :: [Int] -> Bool
kindaSafe report = safe report || any safe reports where
    len = length report
    reports = map (`remove` report) [0..len - 1]


safe :: [Int] -> Bool
safe report = safePair $ pairs report

safePair :: [(Int, Int)] -> Bool
safePair report = all close report && (all increasing report || all decreasing report) where
    close (a, b) = abs (a - b) <= 3
    increasing (a, b) = b > a
    decreasing (a, b) = b < a
