module DeepZoom
( descriptorXml
, mkPyramid
, Pyramid
, pyramidHeight
, pyramidTileFormat
, pyramidTileOverlap
, pyramidTileSize
, pyramidWidth
, savePyramid
, TileFormat(..)
) where

-- | The tile file format.
data TileFormat = PNG | JPEG deriving (Eq)

instance Show TileFormat where
    show JPEG = "jpg"
    show PNG = "png"

-- | The image pyramid.
data Pyramid = Pyramid
    { pyramidWidth :: Int -- ^ The width of the image pyramid.
    , pyramidHeight :: Int -- ^ The height of the image pyramid.
    , pyramidTileSize :: Int -- ^ The tile size of the image pyramid.
    , pyramidTileOverlap :: Int -- ^ The tile overlap of the image pyramid.
    , pyramidTileFormat :: TileFormat -- ^ The tile format of the image pyramid.
    } deriving (Eq, Show)

mkPyramid :: Int -- ^ width
          -> Int -- ^ height
          -> Int -- ^ tile size
          -> Int -- ^ tile overlap
          -> TileFormat -- ^ tile format
          -> Pyramid
mkPyramid width height tileSize tileOverlap tileFormat =
    Pyramid width height tileSize tileOverlap tileFormat

-- | Returns the descriptor XML of a 'Pyramid'.
descriptorXml :: Pyramid -> String
descriptorXml (Pyramid width height tileSize tileOverlap tileFormat) =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++
    "<Image xmlns=\"http://schemas.microsoft.com/deepzoom/2008\"\n" ++
    "       Format=\"" ++ show tileFormat ++ "\"\n" ++
    "       Overlap=\"" ++ show tileOverlap ++ "\"\n" ++
    "       TileSize=\"" ++ show tileSize ++ "\">\n" ++
    "    <Size Height=\"" ++ show height ++
           "\" Width=\"" ++ show width ++ "\"/>\n" ++
    "</Image>\n"

savePyramid :: Pyramid
            -> FilePath -- ^ Destination of `.dzi` file.
            -> IO ()
savePyramid pyramid destination = do
    writeFile destination xml
        where xml = descriptorXml pyramid
