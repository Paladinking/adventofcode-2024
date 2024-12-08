import Data.List (groupBy, sortBy)
import Data.Set (fromList)


main :: IO()
main = do
    input <- readFile "input/input8.txt"
    let content = lines input
    let w = length $ head content
    let h = length content
    let grid = concat content

    let nodes = getNodes grid w h

    let antis = fromList $ concatMap (findAntis w h) nodes
    let antis2 = fromList $ concatMap (findAntis2 w h) nodes

    print $ length antis
    print $ length antis2


getNodes :: String -> Int -> Int -> [[(Int, Int)]]
getNodes grid w h = nodes where
    coords = [(x, y) | x <- [0..w - 1], y <- [0..h - 1]]
    at (x, y) = grid !! (x + w * y)
    chars = filter (\(c, _) -> c /= '.') $ map (\p -> (at p, p)) coords
    sorted = sortBy (\(a, _) (b, _) -> compare a b) chars
    nodes = map (map snd) $ groupBy (\x y -> fst x == fst y) sorted


findAntis :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
findAntis w h nodes = antis where
    pairs = [(x, y) | x <- nodes, y <- nodes, x /= y]
    acc (p1, p2) lst = lst ++ [fst n, snd n] where
        n = findAnti p1 p2
    all_antis = foldr acc [] pairs
    antis = filter (\(x, y) -> x >= 0 && y >= 0 && x < w && y < h) all_antis


findAnti :: (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
findAnti (x1, y1) (x2, y2) = anti where
    dx = x2 - x1
    dy = y2 - y1
    anti = ((x1 - dx, y1 - dy), (x2 + dx, y2 + dy))


findAntis2 :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
findAntis2 w h nodes = antis where
    pairs = [(x, y) | x <- nodes, y <- nodes, x /= y]
    find = uncurry $ findAnti2 w h
    antis = concatMap find pairs


findAnti2 :: Int -> Int -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
findAnti2 w h (x1, y1) (x2, y2) = antis where
    delta_x = x2 - x1
    delta_y = y2 - y1
    anti x y dx dy
      | x < 0 || y < 0 || x >= w || y >= h = []
      | otherwise = (x, y):anti (x + dx) (y + dy) dx dy
    antis = anti x1 y1 delta_x delta_y ++ anti x1 y1 (-delta_x) (-delta_y)
