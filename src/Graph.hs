{-# LANGUAGE OverloadedStrings #-}

module Graph where

import           Control.Lens
import           Data.Bits
import           Data.Char
import           Data.Colour
import           Data.Colour.Names
import           Data.Default.Class
import           Data.Foldable
import qualified Data.List.Extra                        as L
import           Data.Maybe
import           Data.Semigroup
import           Data.Sequences                         (groupAllOn)
import qualified Data.Text                              as T
import qualified Data.Vector                            as V
import           Data.Vector.Algorithms.Merge           as V
import qualified Data.Vector.Sized                      as VS
import           Data.Word
import           FrameBits
import           Graphics.Rendering.Chart
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import           Numeric
import           Numeric.Natural


signal :: [Double] -> [(Double,Double)]
signal xs = [ (x,(sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))) | x <- xs ]

-- graphAll :: String -> [a -> Double] -> V.Vector a -> IO ()
-- graphAll prefix fs dat =
--   forM_ (V.indexed ((\f -> f <$> dat) <$> V.fromList fs)) $ \(i, s) -> do
--     toFile (FileOptions (1000,1000) SVG) (prefix ++ show i ++ ".svg") $ do
--       let ps :: [(Double, Double)]
--           ps = zip [0..] (fmap realToFrac . V.toList $ s)
--       setColors [opaque blue, opaque red]
--       -- layout_y_axis . laxis_generate .= axis
--       layout_y_axis . laxis_override .= axisGridAtTicks
--       -- layout_x_axis . laxis_generate .= axis
--       layout_x_axis . laxis_override .= axisGridAtTicks
--       plot (line "" [ps])
--       plot (points "" ps)

graphAll :: T.Text -> V.Vector (T.Text, (Double, Double)) -> IO ()
graphAll prefix ps =
  forM_ (partitionGraphs ps) $ \(n, v) -> do
    toFile (FileOptions (1000,1000) SVG) (T.unpack $ prefix <> "-" <> n <> ".svg") $ do
      let ps :: [(Double, Double)]
          ps = V.toList v
      setColors [opaque blue, opaque red]
      layout_y_axis . laxis_override .= axisGridAtTicks
      layout_x_axis . laxis_override .= axisGridAtTicks
      plot (line "" [ps])
      plot (points "" ps)

partitionGraphs :: Eq a => V.Vector (a, b) -> V.Vector (a, V.Vector b)
partitionGraphs ps =
  let as = groupAllOn fst ps
  in V.fromList $ (\v -> (fst (V.head v), snd <$> v)) <$> as

colorList = take 100 (cycle [red, orange, yellow, green, blue, indigo])

transpose :: V.Vector (V.Vector a) -> V.Vector (V.Vector a)
transpose = V.fromList . fmap V.fromList . L.transpose . fmap V.toList . V.toList

hist :: [Integer] -> IO ()
hist dat =
  toFile (FileOptions (800, 800) SVG) "hist.svg" $
    plot $ fmap histToPlot $ liftEC $ do
      plot_hist_bins .= 1000
      plot_hist_values .= (fmap realToFrac dat :: [Double])
      plot_hist_norm_func .= const id
