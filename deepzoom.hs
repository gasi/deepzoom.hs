import Graphics.GD
import System.Directory
import System.FilePath
import System.IO

-- Rectangle (left, top, right, bottom)
data Rectangle = Rectangle (Int, Int, Int, Int)
                 deriving (Eq)

instance Show Rectangle
   where show (Rectangle (l, t, r, b)) = "(" ++ show l ++ ", " ++ show t ++ ", " ++ show (r - l) ++ ", " ++ show (b - t) ++ ")"

right :: Rectangle -> Int
right (Rectangle (_, _, width, _)) = width

bottom :: Rectangle -> Int
bottom (Rectangle (_, _, _, height)) = height

left :: Rectangle -> Int
left (Rectangle (x, _, _, _)) = x

top :: Rectangle -> Int
top (Rectangle (_, y, _, _)) = y


-- Image pyramid
levels :: Rectangle -> [Rectangle]
levels (Rectangle (0, 0, 1, 1)) = [Rectangle (0, 0, 1, 1)]
levels bounds = [bounds] ++ (levels (Rectangle (0, 0, newWidth, newHeight)))
    where newWidth = ceiling $ (fromIntegral width) / 2
          newHeight = ceiling $ (fromIntegral height) / 2
          width = right bounds
          height = bottom bounds
  
tiles :: Rectangle -> Int -> Int -> [Rectangle]
tiles bounds size overlap = flatten (map (\x -> rows x size overlap) cs)
    where cs = columns bounds size overlap

columns :: Rectangle -> Int -> Int -> [Rectangle]
columns (Rectangle (left, _, right, _)) _ _
    | width <= 0 = []
    where width = right - left
columns (Rectangle (left, top, right, bottom)) size overlap = [firstBounds] ++ (columns secondBounds size overlap)
    where firstLeft    = max 0 (left - overlap)
          firstRight   = min right (left + size + overlap)
          secondLeft   = min right (left + size)
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
        secondTop    = min bottom (top + size)
        secondBottom = bottom
        firstBounds  = Rectangle (left, firstTop, right, firstBottom)
        secondBounds = Rectangle (left, secondTop, right, secondBottom)

-- Create Deep Zoom XML manifest
descriptorXML :: Int -> Int -> Int -> Int -> String -> String
descriptorXML width height tileSize tileOverlap tileFormat = 
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
    \<Image Format=\"" ++ tileFormat ++ "\" Overlap=\"" ++ show tileOverlap ++ "\" TileSize=\"" ++ show tileSize ++ "\" xmlns=\"http://schemas.microsoft.com/deepzoom/2008\">\
    \<Size Height=\"" ++ show height ++ "\" Width=\"" ++ show width ++ "\"/>\
    \</Image>"

-- List helper
flatten :: [[a]] -> [a]
flatten [] = []
flatten (y:ys) = y ++ (flatten ys)

-- Create path if does not exist
createPath :: String -> IO ()
createPath path = do
    result <- doesDirectoryExist path
    if not result
        then createDirectory path
        else return ()

-- Main
main = do
    -- Load image
    image <- loadJpegFile input
    (width, height) <- imageSize image
    -- Create tiles folder
    createPath tilesPath
    let numLevels = length $ levels (Rectangle (0, 0, width, height)) in
        putStrLn $ show [1..numLevels]
    -- Write descriptor
    --writeFile descriptorFileName (descriptorXML width height tileSize tileOverlap tileFormat)
    putStrLn "Done."
        where input = "test.jpg"
              tileSize = 254
              tileOverlap = 3
              tileFormat = "jpg"
              baseName = dropExtension input
              tilesPath = baseName ++ "_files"
              descriptorFileName = addExtension baseName "dzi"