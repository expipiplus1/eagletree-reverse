{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Main
  ( main
  ) where

import           Codec.FFmpeg
import           Codec.FFmpeg.Juicy
import           Codec.Picture
import           Codec.Picture.Extra
import           Codec.Picture.Types
import           Conduit
import           Control.Exception
import           Data.Bifunctor
import           Data.Conduit.List
import           Data.Semigroup
import           Data.Vector.Sized
import           FrameBits
import           Numeric.Natural
import           Say
import           System.Environment
import           System.Exit

main :: IO ()
main =
  getArgs >>= \case
    [videoFile] -> decode videoFile
    _ -> do
      sayErr "Usage eagletree-decode file"
      exitFailure

decode :: FilePath -> IO ()
decode videoFile =
  runResourceT . runConduit $
  (frames (File videoFile) =$= mapC (first cropBitPixels) =$= number =$= mapC (\(index, (image, time)) -> readBits image time index) =$= printBits)
  -- (frames (File videoFile) =$= mapC (first cropBitPixels) =$= mapC fst =$= number =$= saveImage)

readBits :: Image Pixel8 -> Double -> Natural -> FrameBits
readBits image = FrameBits (generate (\row -> generate (\col -> pixelAt image col row > maxBound `div` 2)))

cropBitPixels :: Image Pixel8 -> Image Pixel8
cropBitPixels = extractLumaPlane . scaleBilinear 23 5 . promoteImage . crop 0 0 maxBound 10

printBits :: MonadIO m => Consumer FrameBits m ()
printBits = mapFoldable extractWords =$= printC

saveImage :: MonadIO m => Consumer (Natural, Image Pixel8) m ()
saveImage = mapM_C (\(index, image) -> liftIO $ writePng (show index <> ".png") image)

saveImageTime :: MonadIO m => Consumer (Image Pixel8, Double) m ()
saveImageTime = mapM_C (\(image, time) -> liftIO $ writePng (show time <> ".png") image)

number :: Monad m => Conduit a m (Natural, a)
number = go 0
  where
    go n = awaitForever (\x -> yield (n, x) *> go (succ n))

frames :: (MonadIO m, MonadResource m, JuicyPixelFormat p) => InputSource -> Producer m (Image p, Double)
frames source =
  bracketP
    (initFFmpeg >> imageReaderTime source)
    (\(_, cleanup) -> liftIO cleanup)
    (\(getFrame, _) -> let loop = liftIO getFrame >>= \case
                             Nothing -> pure ()
                             Just f -> yield f >> loop
                      in loop)
