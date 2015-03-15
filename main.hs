import DeepZoom
import Graphics.GD
import System.Environment


main :: IO ()
main = do
    [source, destination] <- getArgs
    convert source destination 254 1
    where
        convert source destination tileSize tileOverlap = do
            image <- loadJpegFile source
            (w, h) <- imageSize image
            let pyramid = mkPyramid w h tileSize tileOverlap JPEG
            savePyramid pyramid destination
