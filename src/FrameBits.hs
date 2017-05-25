{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds      #-}

module FrameBits
  ( FrameBits(..)
  , extractWords
  ) where

import           Data.Bits
import           Data.Semigroup
import           Data.Vector.Sized as VS
import           Data.Word
import           Numeric.Natural

data FrameBits = FrameBits
  { frameBitsBits  :: Vector 5 (Vector 23 Bool)
  , frameBitsTime  :: Double
  , frameBitsIndex :: Natural
  }
  deriving(Show)

extractWords :: FrameBits -> Vector 5 (Maybe Word16)
extractWords (FrameBits bits t i) = fmap (findNothing . bitsToNat . VS.reverse) bits
  where
    err :: String -> a
    err s = error (s <> " at time " <> show t <> " and index " <> show i)
    findNothing :: Natural -> Maybe Word16
    findNothing n
      | n `shiftR` 18 /= 0 = err ("trailing bits " <> show n)
      | otherwise =
          if n == bit 0
            then Nothing
            else if testBit n 0
                   then err "bit 0 and others set"
                   else if testBit n 1
                          then Just (fromIntegral n)
                          else err ("neither bit 1 nor 0 set " <> show n)

bitsToNat :: Vector n Bool -> Natural
bitsToNat = foldl' (\n b -> (n `shiftL` 1) + if b then 1 else 0) 0
