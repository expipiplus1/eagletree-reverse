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
extractWords (FrameBits bits t i) = fmap (findNothing . bitsToNat) bits
  where
    err :: String -> a
    err s = error (s <> " at time " <> show t <> " and index " <> show i)
    findNothing :: Natural -> Maybe Word16
    findNothing n
      | n .&. 0b11111 /= 0 = err ("trailing bits " <> show n)
      | otherwise =
          let -- shift out the lower 5 blank bits
              shifted = n `shiftR` 5
          in if shifted == bit 17
               then Nothing
               else if testBit shifted 17
                      then err "bit 17 and others set"
                      else if testBit shifted 16
                             then Just (fromIntegral shifted)
                             else err ("neither bit 17 nor 16 set " <> show shifted)

bitsToNat :: Vector n Bool -> Natural
bitsToNat = foldl' (\n b -> (n `shiftL` 1) + if b then 1 else 0) 0
