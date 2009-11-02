import Graphics.GD
import Data.Char

data Rectangle = Rectangle (Int, Int, Int, Int)
                 deriving (Eq, Show)

tileBounds :: Rectangle -> Int -> Int -> [Rectangle]
tileBounds (Rectangle (0, 0, 1, 1)) _ _ = []
{-
    tileBounds bounds tileSize tileOverlap = 
    [Rectangle (tileSize + getX bounds, tile ]
-}

numLevels :: Rectangle -> Int
numLevels bounds = ceiling $ logBase 2 (fromIntegral (max width height))
    where (width, height) = (getWidth bounds, getHeight bounds)

levels :: Rectangle -> [Rectangle]
levels (Rectangle (0, 0, 1, 1)) = [Rectangle (0, 0, 1, 1)]
levels bounds = [bounds] ++ (levels (Rectangle (0, 0, newWidth, newHeight)))
    where newWidth = ceiling $ (fromIntegral width) / 2
          newHeight = ceiling $ (fromIntegral height) / 2
          width = getWidth bounds
          height = getHeight bounds

numColumns :: Rectangle -> Int -> Int
numColumns bounds tileSize = ceiling $ fromIntegral width / fromIntegral tileSize
    where width = getWidth bounds

getWidth :: Rectangle -> Int
getWidth (Rectangle (_, _, width, _)) = width

getHeight :: Rectangle -> Int
getHeight (Rectangle (_, _, _, height)) = height

getX :: Rectangle -> Int
getX (Rectangle (x, _, _, _)) = x

getY :: Rectangle -> Int
getY (Rectangle (_, y, _, _)) = y

main = do
    --image_file <- loadJpegFile "auschwitz.jpg"
    --(w, h) <- imageSize image_file
    --(inWidth, inHeight) <- imageSize image
    --image <- resizeImage outWidth outHeight image
    --    where outWidth = ceiling (inWidth / 2)
    --          outHeight = ceiling (inHeight / 2)
    --saveJpegFile 95 "hell.jpg" image
    --putStrLn $ show (w, h)
--    putStrLn $ show $ tileBounds (Rectangle (0, 0, 1000, 1000)) 254 1
    putStrLn $ show $ numColumns input 254
    putStrLn $ show $ numLevels input
    putStrLn $ show $ levels input
    putStrLn "Done."
		where input = Rectangle (0, 0, 1024, 768)
    
