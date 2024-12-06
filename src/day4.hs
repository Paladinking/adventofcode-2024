main :: IO()
main = do
    input <- readFile "input/input4.txt"
    let content = lines input
    let grid = concat content

    let w = length $ head content
    let h = length content

    print $ countXmas grid w h
    print $ countMasX grid w h


atPos :: String -> Int -> Int -> Int -> Int -> Maybe Char
atPos s x y w h | (x < 0) || (y < 0) || (x >= w) || (y >= h) = Nothing 
atPos s x y w h = Just $ s !! (w * y + x)


dirHasXmas :: String -> Char -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
dirHasXmas s c (x, y) (dx, dy) (w, h) | c == 'S' = atPos s x y w h == Just c
dirHasXmas s c (x, y) (dx, dy) (w, h) = has_xmas where
    nc = case c of
           'M' -> 'A'
           'A' -> 'S'
           _ -> error "Unreachable"
    has_xmas = (atPos s x y w h == Just c) && 
        dirHasXmas s nc (x + dx, y + dy) (dx, dy) (w, h)


posCountXmas :: String -> Int -> Int ->  Int -> Int -> Int
posCountXmas s x y w h | atPos s x y w h /= Just 'X' = 0
posCountXmas s x y w h = has_xmas where
    coords = [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]
    has_xmas = length $ filter (\(dx, dy) -> 
        dirHasXmas s 'M' (x + dx, y + dy) (dx, dy) (w, h)) coords


countXmas :: String -> Int -> Int -> Int
countXmas s w h = res where
    coords = [(x, y) | x <- [0..w - 1], y <- [0..h - 1]]
    counts = map (\(x, y) -> posCountXmas s x y w h) coords
    res = sum counts


posHasMasX :: String -> Int -> Int -> Int -> Int -> Bool
posHasMasX s x y w h | atPos s x y w h /= Just 'A' = False
posHasMasX s x y w h = first && second where
    first = case (atPos s (x - 1) (y - 1) w h, atPos s (x + 1) (y + 1) w h) of
        (Just 'M', Just 'S') -> True
        (Just 'S', Just 'M') -> True
        _ -> False
    second = case (atPos s (x + 1) (y - 1) w h, atPos s (x - 1) (y + 1) w h) of
        (Just 'M', Just 'S') -> True
        (Just 'S', Just 'M') -> True
        _ -> False


countMasX :: String -> Int -> Int -> Int
countMasX s w h = res where
    coords = [(x, y) | x <- [0..w - 1], y <- [0..h - 1]]
    res = length $ filter (\(x, y) -> posHasMasX s x y w h) coords
 
