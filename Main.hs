{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Codec.FFmpeg
import           Codec.Picture.Types
import           Say
import           System.Environment
import           System.Exit

main :: IO ()
main = do
  getArgs >>= \case
    [videoFile] -> do
      decode videoFile
    _ -> do
      sayErr "Usage eagletree-decode file"
      exitFailure

decode :: FilePath -> IO ()
decode video = do
  initFFmpeg
  (read, cleanup) <- imageReaderTime (File video)
  read :: IO (Maybe (Image Pixel8))
  print 1
  cleanup
