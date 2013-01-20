import Graphics.GD

import System.Directory
import System.Environment
import System.FilePath
import System.IO



data Bounds = Bounds {
              left :: Int,
              top :: Int,
              right :: Int,
              bottom :: Int
            } deriving (Eq, Generic, Show)


width :: Bounds -> Int
width r =  (right r) - (left r)

height :: Bounds -> Int
height r =  (bottom r) - (top r)


-- Image pyramid
levels :: Bounds -> [Bounds]
levels (Bounds 0 0 1 1) = [Bounds 0 0 1 1]
levels bounds = [bounds] ++ levels (Bounds 0 0 w h)
    where w = lowerLevel $ width bounds
          h = lowerLevel $ height bounds
          lowerLevel x = ceiling $ (fromIntegral x) / 2

columns :: Bounds -> Int -> Int -> [Bounds]
columns (Bounds l _ r _) _ _
    | w <= 0 = []
    where w = r - l
columns (Bounds l t r b) size overlap = [x] ++ (columns xs size overlap)
    where l'  = max 0 (l - overlap)
          r'  = min r (l + size + overlap)
          l'' = min r (l + size)
          r'' = r
          x  = Bounds l' t r' b
          xs = Bounds l'' t r'' b

rows :: Bounds -> Int -> Int -> [Bounds]
rows (Bounds _ top _ bottom) _ _
  | height <= 0 = []
  where height = bottom - top
rows (Bounds left top right bottom) size overlap = [firstBounds] ++ (rows secondBounds size overlap)
  where firstTop     = max 0 (top - overlap)
        firstBottom  = min bottom (top + size + overlap)
        secondTop    = min bottom (top + size)
        secondBottom = bottom
        firstBounds  = Bounds left firstTop right firstBottom
        secondBounds = Bounds left secondTop right secondBottom

tiles :: Bounds -> Int -> Int -> [Bounds]
tiles bounds size overlap = concat (map (\x -> rows x size overlap) cs)
    where cs = columns bounds size overlap

pyramid :: Bounds -> Int -> Int -> [(Int, Bounds, [Bounds])]
pyramid bounds tileSize tileOverlap = zip3 (reverse [0..maxLevel]) levelBounds tileBounds
    where numLevels = length levelBounds
          maxLevel = numLevels - 1
          levelBounds = levels (Bounds 0 0 (right bounds) (bottom bounds))
          tileBounds = map (\x -> tiles x tileSize tileOverlap) levelBounds

-- Deep Zoom XML manifest
descriptorXML :: Int -> Int -> Int -> Int -> String -> String
descriptorXML width height tileSize tileOverlap tileFormat =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++
    "<Image Format=\"" ++ tileFormat ++
        "\" Overlap=\"" ++ show tileOverlap ++
        "\" TileSize=\"" ++ show tileSize ++
        "\" xmlns=\"http://schemas.microsoft.com/deepzoom/2008\">\n" ++
    "    <Size Height=\"" ++ show height ++ "\" Width=\"" ++ show width ++ "\"/>\n" ++
    "</Image>\n"

level (l, _, _) = l
levelBounds (_, b, _) = b


-- Main
main :: IO ()
main = do
    [source, destination] <- getArgs
    image <- loadJpegFile source
    (w, h) <- imageSize image
    let tileSize = 254
    let tileOverlap = 3
    putStrLn $ show $ levels (Bounds 0 0 w h)
    putStrLn $ show $ tiles (Bounds 0 0 w h) tileSize tileOverlap
    putStrLn $ show $ pyramid (Bounds 0 0 w h) tileSize tileOverlap
