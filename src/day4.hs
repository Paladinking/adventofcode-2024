import Data.ByteString (ByteString, index)
import Data.ByteString.Char8 (pack)
import Data.Word (Word8)


main :: IO()
main = do
    input <- readFile "input/input4.txt"
    let content = lines input
    let grid = pack $ concat content

    let w = length $ head content
    let h = length content

    print $ countXmas grid w h
    print $ countMasX grid w h


atPos :: ByteString -> Int -> Int -> Int -> Int -> Maybe Word8
atPos _ x y w h | (x < 0) || (y < 0) || (x >= w) || (y >= h) = Nothing
atPos s x y w _ = Just $ index s (w * y + x)


dirHasXmas :: ByteString -> Word8 -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
dirHasXmas s c (x, y) _ (w, h) | c == 83 = atPos s x y w h == Just c
dirHasXmas s c (x, y) (dx, dy) (w, h) = has_xmas where
    nc = case c of
           77 -> 65
           65 -> 83
           _ -> error "Unreachable"
    has_xmas = (atPos s x y w h == Just c) && 
        dirHasXmas s nc (x + dx, y + dy) (dx, dy) (w, h)


posCountXmas :: ByteString -> Int -> Int ->  Int -> Int -> Int
posCountXmas s x y w h | atPos s x y w h /= Just 88 = 0
posCountXmas s x y w h = has_xmas where
    coords = [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]
    has_xmas = length $ filter (\(dx, dy) -> 
        dirHasXmas s 77 (x + dx, y + dy) (dx, dy) (w, h)) coords


countXmas :: ByteString -> Int -> Int -> Int
countXmas s w h = res where
    coords = [(x, y) | x <- [0..w - 1], y <- [0..h - 1]]
    counts = map (\(x, y) -> posCountXmas s x y w h) coords
    res = sum counts


posHasMasX :: ByteString -> Int -> Int -> Int -> Int -> Bool
posHasMasX s x y w h | atPos s x y w h /= Just 65 = False
posHasMasX s x y w h = first && second where
    first = case (atPos s (x - 1) (y - 1) w h, atPos s (x + 1) (y + 1) w h) of
        (Just 77, Just 83) -> True
        (Just 83, Just 77) -> True
        _ -> False
    second = case (atPos s (x + 1) (y - 1) w h, atPos s (x - 1) (y + 1) w h) of
        (Just 77, Just 83) -> True
        (Just 83, Just 77) -> True
        _ -> False


countMasX :: ByteString -> Int -> Int -> Int
countMasX s w h = res where
    coords = [(x, y) | x <- [0..w - 1], y <- [0..h - 1]]
    res = length $ filter (\(x, y) -> posHasMasX s x y w h) coords
 
