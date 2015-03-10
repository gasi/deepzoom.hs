import DeepZoom
import Graphics.GD
import System.Environment


main :: IO ()
main = do
    [source] <- getArgs
    image <- loadJpegFile source
    (w, h) <- imageSize image
    let levelBounds = levels (Bounds 0 0 w h)
    levelImages <- mapM (levelImage image) levelBounds
    let ls = zip [1..] levelImages
    mapM_ (\(i, img) -> saveJpegFile 95 (levelPath i) img) ls
    where
        levelImage image b = resizeImage (width b) (height b) image
        levelPath i = show i ++ ".jpg"
