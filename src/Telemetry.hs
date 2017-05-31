{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}

module Telemetry
  where

import           Conduit
import           Control.Applicative
import           Control.Arrow             ((&&&))
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Data.Bits
import qualified Data.ByteString.Char8     as BS8
import qualified Data.Conduit.List         as CL
import           Data.Foldable
import           Data.Int
import           Data.List.Extra           (chunksOf, genericLength, sortOn)
import qualified Data.List.NonEmpty        as NE
import           Data.Maybe
import           Data.Semigroup
import qualified Data.Text                 as T
import qualified Data.Vector               as V
import           Data.Word
import           GHC.Base                  (Int (..))
import           GHC.Integer.Logarithms
import           Graph
import           Numeric
import           Numeric.Natural
import           Say

import           Parse

parseFile :: FilePath -> IO (V.Vector Parse.Packet)
parseFile file =
  runResourceT . runConduit $
  (sourceFile file =$= linesUnboundedAsciiC =$= mapC readBS =$=
   splitBytes =$=
   Parse.parse =$=
   sinkVector)

bs :: IO (V.Vector Parse.Packet)
bs = do
  a <- parseFile "bits"
  b <- parseFile "bits2"
  c <- parseFile "bits3"
  pure $ mconcat [a, b, c]

readBS :: Read a => BS8.ByteString -> a
readBS = read . BS8.unpack

splitBytes :: Monad m => Conduit (Maybe Word16) m (Maybe Word8)
splitBytes = concatMapC $ \case
  Nothing -> [Nothing] :: [Maybe Word8]
  Just x -> [Just (fromIntegral (x `shiftR` 8)), Just (fromIntegral x)]

swapPairs :: Monad m => Conduit (Maybe Word16) m (Maybe Word16)
swapPairs = CL.chunksOf 2 =$= mapC reverse =$= concatC

show' :: Show a => a -> T.Text
show' = T.pack . show

graph :: V.Vector Packet -> IO ()
graph ps =
  let expand = \case
        (_, Unknown _) -> []
        (n, Type3 a b c d e f g) ->
          let x = fromIntegral n
          in [ ("0303-a", (x, fromIntegral a))
             , ("0303-b", (x, fromIntegral b))
             , ("0303-c", (x, fromIntegral c))
             , ("0303-d", (x, fromIntegral d))
             , ("0303-e", (x, fromIntegral e))
             , ("0303-f", (x, fromIntegral f))
             , ("0303-g", (x, fromIntegral g))
             ]
        (n, Type2 a b c d e f g) ->
          let x = fromIntegral n
          in [ ("02-a", (x, fromIntegral a))
             , ("02-b", (x, fromIntegral b))
             , ("02-c", (x, fromIntegral c))
             , ("02-d", (x, fromIntegral d))
             , ("02-e", (x, fromIntegral e))
             , ("02-f", (x, fromIntegral f))
             , ("02-g", (x, fromIntegral g))
             ]
        (n, Type1 a) ->
          let x = fromIntegral n
          in [ ("01-a", (x, fromIntegral a))
             ]
        (n, Type50 a) ->
          let x = fromIntegral n
          in [ ("50-a", (x, fromIntegral a))
             ]
        (n, Type0000 a b c d e) ->
          let x = fromIntegral n
          in [ ("00-a", (x, fromIntegral a))
             , ("00-b", (x, fromIntegral b))
             , ("00-c", (x, fromIntegral c))
             , ("00-d", (x, fromIntegral d))
             , ("00-e", (x, fromIntegral e))
             ]
        (n, TypeA7 a b c d e f g h i) ->
          let x = fromIntegral n
          in [ ("a7-a", (x, fromIntegral a))
             , ("a7-b", (x, fromIntegral b))
             , ("a7-c", (x, fromIntegral c))
             , ("a7-d", (x, fromIntegral d))
             , ("a7-e", (x, fromIntegral e))
             , ("a7-f", (x, fromIntegral f))
             , ("a7-g", (x, fromIntegral g))
             , ("a7-h", (x, fromIntegral h))
             , ("a7-i", (x, fromIntegral i))
             ]
        (n, Type3D a b c2 d1 d2 e f f' g1) ->
          let x = fromIntegral n
          in (case a of
               Nothing -> []
               Just a' -> [ ("3d-a", (x, fromIntegral a')) ]) V.++
             [ ("3d-b", (x, fromIntegral b))
             , ("3d-c2", (x, fromIntegral c2))
             , ("3d-d1", (x, fromIntegral d1))
             , ("3d-d2", (x, fromIntegral d2))
             , ("3d-e", (x, fromIntegral e))
             , ("3d-f", (x, fromIntegral f))
             , ("3d-f2", (x, fromIntegral f'))
             , ("3d-g1", (x, fromIntegral g1))
             ]
  in graphAll
       "graph"
       (V.concatMap expand . V.indexed $ ps)

sign :: Word16 -> Int16
sign = fromIntegral

unSplit :: V.Vector Word8 -> V.Vector Word16
unSplit = V.fromList . f . V.toList
  where
    f = fmap (\case
                [x] -> fromIntegral x
                [x,y] -> fromIntegral x `shiftL` 8 .|. fromIntegral y
             )
        . chunksOf 2
