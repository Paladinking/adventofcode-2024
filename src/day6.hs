import Data.List (find)
import Data.Maybe (fromJust, isNothing)
import Data.Set (Set, empty, insert, member, toList)
import Data.ByteString (ByteString, index)
import Data.ByteString.Char8 (pack)
import Data.Word (Word8)


main :: IO()
main = do
    input <- readFile "input/input6.txt"
    let content = lines input
    let grid = pack $ concat content

    let w = length $ head content
    let h = length content

    let (gx, gy) = findGuard grid w h
    let path = walk grid w h gx gy 0 (-1)

    let count = length path
    let without_guard = filter (/= (gx, gy)) (toList path)

    let res = length $ filter (hasLoop grid w h gx gy) without_guard

    print count
    print res


atPos :: ByteString -> Int -> Int -> Int -> Int -> Maybe Word8
atPos _ w h x y | (x < 0) || (y < 0) || (x >= w) || (y >= h) = Nothing
atPos s w _ x y = Just $ index s (x + (w * y))


findGuard :: ByteString -> Int -> Int -> (Int, Int)
findGuard s w h = pos where
    coords = [(x, y) | x <- [0..w - 1], y <- [0..h - 1]]
    pos = fromJust $ find (\(x, y) -> atPos s w h x y == Just 94) coords


turn :: (Int, Int) -> (Int, Int)
turn (dx, dy) = case (dx, dy) of 
                   (0, -1) -> (1, 0)
                   (1, 0) -> (0, 1)
                   (0, 1) -> (-1, 0)
                   (-1, 0) -> (0, -1)
                   _ -> (0, 0)


walk :: ByteString -> Int -> Int -> Int -> Int -> Int -> Int -> Set (Int, Int)
walk grid w h x y _ _ | isNothing (atPos grid w h x y) = empty
walk grid w h x y dx dy | atPos grid w h (x + dx) (y + dy) == Just 35 = turned where
    (ndx, ndy) = turn (dx, dy)
    turned = walk grid w h x y ndx ndy
walk grid w h x y dx dy = insert (x, y) next where
    next = walk grid w h (x + dx) (y + dy) dx dy


hasLoop :: ByteString -> Int -> Int -> Int -> Int -> (Int, Int) -> Bool
hasLoop grid w h xp yp (bx, by) = loop xp yp (0, -1) empty where
    blocked x1 y1 = ((x1, y1) == (bx, by)) || (atPos grid w h x1 y1 == Just 35)
    loop x y (dx, dy) visited
      | isNothing (atPos grid w h x y) = False
      | member (x, y, dx, dy) visited = True
      | blocked (x + dx) (y + dy) = loop x y (turn (dx, dy)) visited
      | otherwise = loop (x + dx) (y + dy) (dx, dy) (insert (x, y, dx, dy) visited)
