{-# LANGUAGE DeriveGeneric #-}

module DeepZoom
( Bounds(Bounds)
, columns
, descriptorXML
, height
, level
, levelBounds
, levels
, pyramid
, rows
, tiles
, width
) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)


data Bounds = Bounds {
              left :: Int,
              top :: Int,
              right :: Int,
              bottom :: Int
            } deriving (Eq, Generic, Show)

instance ToJSON Bounds

width :: Bounds -> Int
width r =  right r - left r

height :: Bounds -> Int
height r =  bottom r - top r


-- Image pyramid
levels :: Bounds -> [Bounds]
levels (Bounds 0 0 1 1) = [Bounds 0 0 1 1]
levels bounds = bounds : levels (Bounds 0 0 w h)
  where w = lowerLevel $ width bounds
        h = lowerLevel $ height bounds
        lowerLevel x = ceiling $ fromIntegral x / 2

columns :: Bounds -> Int -> Int -> [Bounds]
columns b _ _
    | width b <= 0 = []
columns (Bounds l t r b) size overlap = x : columns xs size overlap
    where xl  = max 0 (l - overlap)
          xr  = min r (l + size + overlap)
          xsl = min r (l + size)
          xsb = r
          x   = Bounds xl t xr b
          xs  = Bounds xsl t xsb b

rows :: Bounds -> Int -> Int -> [Bounds]
rows b _ _
  | height b <= 0 = []
rows (Bounds l t r b) size overlap = x : rows xs size overlap
  where xl  = max 0 (t - overlap)
        xb  = min b (t + size + overlap)
        xst = min b (t + size)
        xsb = b
        x   = Bounds l xl r xb
        xs  = Bounds l xst r xsb

tiles :: Bounds -> Int -> Int -> [Bounds]
tiles bounds size overlap = concatMap (\x -> rows x size overlap) cs
    where cs = columns bounds size overlap

pyramid :: Bounds -> Int -> Int -> [(Int, Bounds, [Bounds])]
pyramid bounds tileSize tileOverlap = zip3 (reverse [0..maxLevel]) levelBounds tileBounds
    where numLevels = length levelBounds
          maxLevel = numLevels - 1
          levelBounds = levels (Bounds 0 0 (right bounds) (bottom bounds))
          tileBounds = map (\x -> tiles x tileSize tileOverlap) levelBounds

level :: (Int, Bounds, [Bounds]) -> Int
level (l, _, _) = l

levelBounds :: (Int, Bounds, [Bounds]) -> Bounds
levelBounds (_, b, _) = b

-- Deep Zoom XML manifest
descriptorXML :: Int -> Int -> Int -> Int -> String -> String
descriptorXML width height tileSize tileOverlap tileFormat =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++
    "<Image Format=\"" ++ tileFormat ++
    "    \" Overlap=\"" ++ show tileOverlap ++
    "    \" TileSize=\"" ++ show tileSize ++
    "    \" xmlns=\"http://schemas.microsoft.com/deepzoom/2008\">\n" ++
    "    <Size Height=\"" ++ show height ++ "\" Width=\"" ++ show width ++ "\"/>\n" ++
    "</Image>\n"
