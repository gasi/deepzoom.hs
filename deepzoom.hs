import Graphics.GD
import Data.Char

-- Rectangle (left, top, right, bottom)
data Rectangle = Rectangle (Int, Int, Int, Int)
                 deriving (Eq)

instance Show Rectangle
   where show (Rectangle (l, t, r, b)) = "(" ++ show l ++ ", " ++ show t ++ ", " ++ show (r - l) ++ ", " ++ show (b - t) ++ ")"

tiles :: Rectangle -> Int -> Int -> [Rectangle]
tiles bounds size overlap = flatten (map (\x -> rows x size overlap) cs)
    where cs = columns bounds size overlap

flatten :: [[a]] -> [a]
flatten [] = []
flatten (y:ys) = y ++ (flatten ys)

columns :: Rectangle -> Int -> Int -> [Rectangle]
columns (Rectangle (left, _, right, _)) _ _
    | width <= 0 = []
    where width = right - left
columns (Rectangle (left, top, right, bottom)) size overlap = [firstBounds] ++ (columns secondBounds size overlap)
    where firstLeft    = max 0 (left - overlap)
          firstRight   = min right (left + size + overlap)
          secondLeft   = firstRight
          secondRight  = right
          firstBounds  = Rectangle (firstLeft, top, firstRight, bottom)
          secondBounds = Rectangle (secondLeft, top, secondRight, bottom)

rows :: Rectangle -> Int -> Int -> [Rectangle]
rows (Rectangle (_, top, _, bottom)) _ _
  | height <= 0 = []
  where height = bottom - top
rows (Rectangle (left, top, right, bottom)) size overlap = [firstBounds] ++ (rows secondBounds size overlap)
  where firstTop     = max 0 (top - overlap)
        firstBottom  = min bottom (top + size + overlap)
        secondTop    = firstBottom
        secondBottom = bottom
        firstBounds  = Rectangle (left, firstTop, right, firstBottom)
        secondBounds = Rectangle (left, secondTop, right, secondBottom)

levels :: Rectangle -> [Rectangle]
levels (Rectangle (0, 0, 1, 1)) = [Rectangle (0, 0, 1, 1)]
levels bounds = [bounds] ++ (levels (Rectangle (0, 0, newWidth, newHeight)))
    where newWidth = ceiling $ (fromIntegral width) / 2
          newHeight = ceiling $ (fromIntegral height) / 2
          width = getRight bounds
          height = getBottom bounds

getRight :: Rectangle -> Int
getRight (Rectangle (_, _, width, _)) = width

getBottom :: Rectangle -> Int
getBottom (Rectangle (_, _, _, height)) = height

getLeft :: Rectangle -> Int
getLeft (Rectangle (x, _, _, _)) = x

getTop :: Rectangle -> Int
getTop (Rectangle (_, y, _, _)) = y

main = do
    --putStrLn $ show $ levels input
    putStrLn $ show $ {-length $-} tiles input tileSize tileOverlap
    putStrLn "Done."
        where input = Rectangle (0, 0, 600, 500)
              tileSize = 254
              tileOverlap = 1