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
  | Type4 Word8 Word8 Word8 Word8 Word8 Word8
  | TypeV Word16 Word8 Word8 Word8 Word8 Word8
  | Type2 Word16 Word8 Word8 Word8 Word8 Word8 Word8 Word8
  | Type1 Word8 Word8 Word8 Word8 Word16 Word16 Word16 Word8
  | Unknown (V.Vector (ShowHex Word8))
  | Error (V.Vector (ShowHex Word8)) T.Text
  | Blank
  deriving (Show, Eq, Ord)

type P = ConduitParser (Maybe Word8)

parse :: (MonadIO m, MonadThrow m) => Conduit (Maybe Word8) m Packet
parse = do
  dropWhileC isNothing
  CL.sequence (toConsumer (runConduitParser parser))

parser :: MonadIO m => P m Packet
parser = do
  p <- choice [parse4, parse3, parse2, parse1, parseUnknown]
  -- p <- parseUnknown
  skipBlank
  pure p

parse4 :: Monad m => P m Packet
parse4 = do
  expect 0x5
  a <- word

  expect 0x4
  b <- word
  guard (b == 0x8e || b == 0x89 || b == 0x93)

  expect 0x2
  c <- word

  expect 0x0
  d <- word

  expect 0x0
  expect 0x0

  e <- word
  f <- word

  pure $ Type4 a b c d e f


-- 03 a7 03 a7 00 01 02 5f 01 0a 40 41 31 00 66 75 91 48 67 09
-- 03 a7 03 a7 00 01 02 5f 01 0a 96 89 31 00 64 69 91 48 15 09
--       03 a7 00 01 02 5f 01 0a 03 03 31 00 58 37 91 48 01 09 00 49
--       03 a7 00 01 02 5f 01 0a 03 03 31 00 57 15 91 48 68 09 00 45
--       03 a7 00 01 02 5f 01 0a 03 03 31 00 50 01 91 48 39 09 00 26
--       03 a7 00 01 02 5f 01 0a 03 03 31 00 51 54 91 48 97 09 00 25
-- 03 a7 03 a7 00 01 02 5f 01 0a 54 47 31 00 51 91 91 48 89 09
-- 03 a7 03 a7 00 01 02 5f 01 0a 54 05 31 00 52 05 91 48 37 09
-- 03 a7 03 a7 00 01 02 5f 01 0a 26 22 31 00 50 74 91 48 29 09
-- 03 a7 03 a7 00 01 02 5f 01 0a 25 10 31 00 51 00 91 48 16 09
-- 03 a7 03 a7 00 01 02 5f 01 0a 30 78 31 00 63 05 91 48 76 09
-- 03 a7 03 a7 00 01 02 5f 01 0a 38 01 31 00 66 80 91 48 09 09
--       03 a7 00 01 02 5f 01 0a 03 03 31 00 66 77 91 48 79 09 00 17
--       03 a7 00 01 02 5f 01 0a 03 03 31 00 75 12 91 48 17 09 00 26
-- 03 a7 03 a7 00 01 02 5f 01 0a 40 53 31 00 66 76 91 48 74 09
--       03 a7 00 01 02 5f 01 0a 03 03 31 00 66 43 91 48 72 09 00 16
-- 03 a7 03 a7 00 01 02 5f 01 0a 42 69 31 00 75 55 91 48 42 09
--       03 a7 00 01 02 5f 01 0a 03 03 31 00 75 58 91 48 44 09 00 10
--       03 a7 00 01 02 5f 01 0a 03 03 31 00 78 21 91 48 78 09 00 10
--       03 a7 00 01 02 5f 01 0a 03 03 31 00 84 96 91 48 89 09 00 13
-- 03 a7 03 a7 00 01 02 5f 01 0a 40 22 31 00 86 11 91 48 94 09
-- 03 a7 03 a7 00 01 02 5f 01 0a 43 91 31 00 88 11 91 48 26 09
-- 03 a7 03 a7 00 01 02 5f 01 0a 44 63 31 00 88 56 91 48 06 08
--       03 a7 00 01 02 5f 01 0a 03 03 31 00 89 97 91 48 56 09 00 04
--       03 a7 00 01 02 5f 01 0a 03 03 31 00 91 00 91 48 57 09 00 07
-- 03 a7 03 a7 00 01 02 5f 01 0a 50 10 31 00 89 09 91 48 63 09
-- 03 a7 03 a7 00 01 02 5f 01 0a 67 23 31 00 74 42 91 48 92 09
--       03 a7 00 01 02 5f 01 0a 03 03 31 00 75 07 91 48 57 09 00 04

parse2 :: Monad m => P m Packet
parse2 = do
  a <- word16

  expect 0x02
  (b1, b2) <- nibbles
  guard (b1 == 0x1 || b1 == 0x9)
  guard (b2 == 0xf || b2 == 0x9)

  expect 0x1
  (c1, c2) <- nibbles
  guard (c1 == 0x1)
  guard (c2 == 0x1 || c2 == 0x2 || c2 == 0x4)

  expect 0x0
  d <- word

  expect 0x80
  expect 0x0

  e <- word
  f <- word

  pure $ Type2 a b1 b2 c1 c2 d e f

parse1 :: Monad m => P m Packet
parse1 = do
  expect 0x1
  (a1, a2) <- nibbles
  guard (a1 == 0x0 || a1 == 0x8)
  guard (a2 == 0x3)

  expect 0x40
  expect 0x10

  expect 0x0
  expect 0x0

  expect 0x0
  b <- word

  c <- word
  guard (c == 0x0 || c == 0x80)
  expect 0x0

  expect 0x0
  d <- bcd16
  expect 0x0

  e <- word16

  f <- word16

  g <- word
  guard (g == 0x0 || g == 0x80)
  expect 0x0

  pure $ Type1 a1 a2 b c d e f g

parse3 :: Monad m => P m Packet
parse3 = do
  expect 0x3
  expect 0x3

  (a1, a2) <- nibbles
  guard (a1 == 0x8)
  guard (a2 == 0x0 || a2 == 0x3)
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

  pure $ Type3 a2 b c d e f


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

int :: Monad m => P m Int8
int = fromIntegral <$> word

nibbles :: Monad m => P m (Word8, Word8)
nibbles = do
  a <- word
  pure (a `shiftR` 4, a .&. 0xf)

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

is1 :: Packet -> Bool
is1 = \case
  Type1 {} -> True
  _ -> False

is3 :: Packet -> Bool
is3 = \case
  Type3 {} -> True
  _ -> False

is2 :: Packet -> Bool
is2 = \case
  Type2 {} -> True
  _ -> False

is4 :: Packet -> Bool
is4 = \case
  Type4 {} -> True
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
  Blank -> "-----"

prettyPacketR :: Packet -> String
prettyPacketR = \case
  Unknown vs -> L.intercalate " " . fmap show . concatMap (concat . reverse) . chunksOf 2 . chunksOf 2 . V.toList $ vs
  Blank -> "-----"

nubTranspose :: V.Vector Packet -> V.Vector (V.Vector (ShowHex Word8))
nubTranspose ps =
  let us = [V.toList $ u | Unknown u <- V.toList ps]
      ts = L.transpose us
  in V.fromList . fmap (V.fromList . L.sort . nubOrd) $ ts

sortOnThird :: V.Vector Packet -> V.Vector Packet
sortOnThird = V.modify (V.sortBy (comparing (\(Unknown v) -> v V.!? 2)))

filterPenultimate :: Word8 -> V.Vector Packet -> V.Vector Packet
filterPenultimate w = V.filter (\(Unknown v) -> v V.!? (V.length v - 2) == Just (ShowHex w))

isUnknown3N :: Word8 -> Packet -> Bool
isUnknown3N n = \case
  Unknown v
   | (v V.!? 2) == Just (ShowHex n) -> True
  _ -> False

sortReverse :: V.Vector Packet -> V.Vector Packet
sortReverse = V.modify (V.sortBy (comparing (\(Unknown v) -> (v V.!? 10, v V.!? 11))))
