module Graph where

import           Control.Lens
import           Data.Bits
import           Data.Char
import           Data.Colour
import           Data.Colour.Names
import           Data.Colour.Names
import           Data.Default.Class
import           Data.Foldable
import qualified Data.List.Extra                        as L
import           Data.Maybe
import qualified Data.Vector                            as V
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

graphAll :: [a -> Double] -> V.Vector a -> IO ()
graphAll fs dat =
  forM_ (V.indexed ((\f -> f <$> dat) <$> V.fromList fs)) $ \(i, s) -> do
    toFile (FileOptions (1000,1000) SVG) (show i ++ ".svg") $ do
      let ps :: [(Double, Double)]
          ps = zip [0..] (fmap realToFrac . V.toList $ s)
      setColors [opaque blue, opaque red]
      -- layout_y_axis . laxis_generate .= axis
      layout_y_axis . laxis_override .= axisGridAtTicks
      -- layout_x_axis . laxis_generate .= axis
      layout_x_axis . laxis_override .= axisGridAtTicks
      plot (line "" [ps])
      plot (points "" ps)

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
