{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Parse where

import           Conduit                      hiding (await, leftover)
import           Control.Applicative
import           Control.Arrow                ((&&&))
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Data.Bits
import qualified Data.ByteString.Char8        as BS8
import qualified Data.Conduit.List            as CL
import           Data.Conduit.Parser
import           Data.Foldable
import           Data.Int
import qualified Data.List                    as L
import           Data.List.Extra              (chunksOf, genericLength, nubOrd,
                                               sortOn)
import qualified Data.List.NonEmpty           as NE
import qualified Data.Map                     as Map
import           Data.Maybe
import           Data.Ord
import           Data.Semigroup
import qualified Data.Set                     as Set
import qualified Data.Text                    as T
import qualified Data.Vector                  as V
import qualified Data.Vector.Algorithms.Intro as V
import           Data.Word
import           GHC.Base                     (Int (..))
import           GHC.Integer.Logarithms
import           Graph
import           Numeric
import           Numeric.Natural
import           Say
import           Text.Parser.Combinators

newtype ShowHex a = ShowHex { unShowHex :: a }
  deriving(Eq, Ord, Num, Enum, Real, Integral)

instance (Integral a, Show a, FiniteBits a) => Show (ShowHex a) where
  show (ShowHex x) =
    let s = showHex x ""
        pad s n = replicate (n - length s) '0' ++ s
    in pad s (finiteBitSize x `div` 4)

newtype Tag = Tag { unTag :: ShowHex Word8 }
  deriving (Show, Eq, Ord)

data Packet
  = Type3 Word8 Word8 Word16 Word16 Word16 Word16
  | Type5 Word8 Word8 Word8 Word8 Word8 Word8
  | TypeV Word16 Word8 Word8 Word8 Word8 Word8
  | Unknown (V.Vector (ShowHex Word8))
  | Error (V.Vector (ShowHex Word8)) T.Text
  deriving (Show, Eq, Ord)

type P = ConduitParser (Maybe Word8)

parse :: (MonadIO m, MonadThrow m) => Conduit (Maybe Word8) m Packet
parse = do
  dropWhileC isNothing
  CL.sequence (toConsumer (runConduitParser parser))

parser :: MonadIO m => P m Packet
parser = do
  -- p <- choice [parseV, parse3, parse5, parseUnknown]
  p <- choice [parseUnknown]
  skipBlank
  pure p

parse3 :: Monad m => P m Packet
parse3 = do
  expect 0x3
  expect 0x3
  a <- word
  expect 0x10
  expect 0x0
  b <- word
  c <- word16
  expect 0x0
  expect 0x0
  expect 0x0
  d <- bcd16
  expect 0x0
  e <- bcd16
  f <- bcd16
  expect 0x0
  expect 0x1
  pure $ Type3 a b c d e f

parse5 :: Monad m => P m Packet
parse5 = do
  expect 0x5
  a <- word
  expect 0x4
  b <- word
  expect 0x2
  c <- word
  expect 0x0
  d <- word
  expect 0x0
  expect 0x0
  e <- word
  f <- word
  pure $ Type5 a b c d e f

-- parseV :: Monad m => P m Packet
-- parseV = do
--   a <- word16
--   expect 0x2
--   b <- word
--   expect 0x1
--   c <- word
--   expect 0x0
--   d <- word
--   expect 0x80
--   expect 0x0
--   e <- word
--   f <- word
--   pure $ TypeV a b c d e f
parseV :: Monad m => P m Packet
parseV = do
  a <- word16
  t <- word
  guard (testBit t 1)
  b <- word
  choice [expect 0x0, expect 0x14]
  c <- word
  choice [expect 0x0, expect 0x14]
  d <- word
  choice [expect 0x80, expect 0x0]
  expect 0x0
  e <- word
  f <- word
  pure $ TypeV a b c d e f

bcd8 :: Monad m => P m Word8
bcd8 = do
  a <- word
  let upper = a `shiftR` 4
      lower = a .&. 0xf
  guard (upper < 10)
  guard (lower < 10)
  pure $ upper * 10 + lower

bcd16 :: Monad m => P m Word16
bcd16 = do
  a <- fromIntegral <$> bcd8
  b <- fromIntegral <$> bcd8
  pure $ a * 100 + b

word16 :: Monad m => P m Word16
word16 = do
  a <- word
  b <- word
  pure $ fromIntegral a `shiftL` 8 .|. fromIntegral b

expect :: Monad m => Word8 -> P m ()
expect n = void (satisfy (== Just n))

parseUnknown :: Monad m => P m Packet
parseUnknown = do
  v <- some word
  pure $ Unknown (V.fromList (ShowHex <$> v))

word :: Monad m => P m Word8
word =
  await >>= \case
    Nothing -> empty
    Just x -> pure x

skipBlank :: Monad m => P m ()
skipBlank = void $ many (satisfy isNothing)

satisfy :: Monad m => (a -> Bool) -> ConduitParser a m a
satisfy p = do
  n <- await
  if p n
    then pure n
    else empty

--------------------------------------------------------------------------------
-- Data sanitizing
--------------------------------------------------------------------------------

deGapAll :: Integral a => a -> Integer
deGapAll = appEndo (foldMap (Endo . deGap) [5,9..13]) . toInteger

deGap :: Integral a => Integer -> a -> Integer
deGap gapPower x =
  let p = 2 ^ gapPower
      gap = (p * 3) `div` 4
      d = toInteger x `div` (p * 2)
  in toInteger x - d * gap

reverseBits :: Word8 -> Word8
reverseBits i =
  let b :: Word32
      b = fromIntegral i
      r = (((b * 0x0802 .&. 0x22110) .|. (b * 0x8020 .&. 0x88440)) * 0x10101) `shiftR` 16;
  in fromIntegral r

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

isUnknown :: Packet -> Bool
isUnknown = \case
  Unknown _ -> True
  _ -> False

is3 :: Packet -> Bool
is3 = \case
  Type3 {} -> True
  _ -> False

is5 :: Packet -> Bool
is5 = \case
  Type5 {} -> True
  _ -> False

isV :: Packet -> Bool
isV = \case
  TypeV {} -> True
  _ -> False

isFF :: Packet -> Bool
isFF = \case
  Unknown v
   | V.head v == 0xff -> True
  _ -> False

is0 :: Packet -> Bool
is0 = \case
  Unknown v
   | V.head v == 0x0 -> True
  _ -> False

isE :: Packet -> Bool
isE = \case
  Unknown v
   | V.head v == 0xe -> True
  _ -> False

isC' :: Packet -> Bool
isC' = \case
  Unknown v
   | V.head v == 0xc -> True
  _ -> False

isUnknownN :: Word8 -> Packet -> Bool
isUnknownN n = \case
  Unknown v
   | unShowHex (V.head v) == n -> True
  _ -> False

isUnknownNL :: Word8 -> Int -> Packet -> Bool
isUnknownNL n l = \case
  Unknown v
   | unShowHex (V.head v) == n
   , V.length v == l -> True
  _ -> False

isUnknownNNL :: Word8 -> Word8 -> Int -> Packet -> Bool
isUnknownNNL n1 n2 l = \case
  Unknown v
   | unShowHex (v V.! 0) == n1
   , unShowHex (v V.! 1) == n2
   , V.length v == l -> True
  _ -> False

sortUnknowns :: V.Vector Packet -> V.Vector Packet
sortUnknowns = V.modify (V.sortBy (comparing (\(Parse.Unknown v) -> V.head v )))

prettyPacket :: Packet -> String
prettyPacket = \case
  Unknown vs -> L.intercalate " " . fmap show . V.toList $ vs

nubTranspose :: V.Vector Packet -> V.Vector (V.Vector (ShowHex Word8))
nubTranspose ps =
  let us = [V.toList $ u | Unknown u <- V.toList ps]
      ts = L.transpose us
  in V.fromList . fmap (V.fromList . L.sort . nubOrd) $ ts

sortOnThird :: V.Vector Packet -> V.Vector Packet
sortOnThird = V.modify (V.sortBy (comparing (\(Unknown v) -> v V.!? 2)))
