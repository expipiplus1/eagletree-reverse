{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ViewPatterns               #-}

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

import qualified Parse

newtype ShowHex a = ShowHex { unShowHex :: a }
  deriving(Eq, Ord, Num)

instance (Integral a, Show a) => Show (ShowHex a) where
  showsPrec _ = showHex . unShowHex

newtype Tag = Tag { unTag :: ShowHex Word8 }
  deriving (Show, Eq, Ord)

newtype Payload = Payload { unPayload :: V.Vector (ShowHex Word8) }
  deriving (Show)

data Packet
  -- = Type7584 Word16 Word16 Word16 Word16 Word16
  -- | Type49344 Word16 Word16 Word16 Word16 Word16
  --                    Word16 Word16 Word16 Word16
  -- = TypeC Word16 Word16 Word8 Word16 Int16
  -- = TypeC Word16 Word16 Word8 Word16 Word16
  = TypeC Word16 Word16 Word8 Int16 Int16
  | Unknown Payload
  | Error Tag Payload T.Text
  deriving (Show)

parseFile :: FilePath -> IO (V.Vector Packet)
parseFile file =
  runResourceT . runConduit $
  (sourceFile file =$= linesUnboundedAsciiC =$= mapC readBS =$= swapPairs =$= splitBytes =$= parse =$=
   sinkVector)

parseFile' :: FilePath -> IO (V.Vector Parse.Packet)
parseFile' file =
  runResourceT . runConduit $
  (sourceFile file =$= linesUnboundedAsciiC =$= mapC readBS =$= splitBytes =$= Parse.parse =$=
   sinkVector)

readBS :: Read a => BS8.ByteString -> a
readBS = read . BS8.unpack

splitBytes :: Monad m => Conduit (Maybe Word16) m (Maybe Word8)
splitBytes = concatMapC $ \case
  Nothing -> [Nothing] :: [Maybe Word8]
  Just x -> [Just (fromIntegral (x `shiftR` 8)), Just (fromIntegral x)]

swapPairs :: Monad m => Conduit (Maybe Word16) m (Maybe Word16)
swapPairs = CL.chunksOf 2 =$= mapC reverse =$= concatC

parse :: MonadIO m => Conduit (Maybe Word8) m Packet
parse =
  parseTag >>= \case
    Nothing -> pure ()
    Just (Tag 0xc) -> do
      parseC >>= \case
        Nothing -> pure ()
        Just p -> yield p
      parse
    -- Just (Tag 49344) -> do
    --   parse49344 >>= \case
    --     Nothing -> pure ()
    --     Just p -> yield p
    --   parse
    Just (Tag unknownTag) -> do
      Payload payload <- parseUnknownPayload
      yield (Unknown (Payload $ unknownTag `V.cons` payload))
      parse

parseC :: MonadIO m => ConduitM (Maybe Word8) o m (Maybe Packet)
parseC = runMaybeC $ do
  expect 0xe
  expect 0x0
  expect 0x42
  expect 0x0

  expect 0x2
  a <- parse16
  expect 0x0

  expect 0x2
  b <- fromInteger . deGapAll <$> parse16
  c <- fromInteger . deGapAll <$> parse8
  expect 0x2
  d <- fromInteger . deGapAll . (fromIntegral :: Word16 -> Int16) <$> parse16
  e <- fromInteger . deGapAll . (fromIntegral :: Word16 -> Int16) <$> parse16
  expect 0x0
  expect 0x6
  pure $ TypeC a b c d e

deGapAll :: Integral a => a -> Integer
deGapAll = appEndo (foldMap (Endo . deGap) [5,9..13]) . toInteger

deGap :: Integral a => Integer -> a -> Integer
deGap gapPower x =
  let p = 2 ^ gapPower
      gap = (p * 3) `div` 4
      d = toInteger x `div` (p * 2)
  in toInteger x - d * gap

wrapAt :: Integral a => Integer -> a -> Integer
wrapAt bound (toInteger -> x) =
  if x > bound
    then x - bound
    else x

nextPow2 :: Integer -> Integer
nextPow2 = (2^) . toInteger . clog2 . fromInteger

clog2 :: Natural -> Natural
clog2 x = flog2 (x * 2 - 1)

flog2 :: Natural -> Natural
flog2 x = fromIntegral (I# (integerLog2# (toInteger x)))

parse8 :: Monad m => ConduitM (Maybe Word8) o (MaybeT m) Word8
parse8 =
  await >>= \case
    Just (Just x) -> pure x
    _ -> lift empty

parse16 :: Monad m => ConduitM (Maybe Word8) o (MaybeT m) Word16
parse16 = do
  first <- await
  second <- await
  case (first, second) of
    (Just (Just a), Just (Just b))
      -> pure $ fromIntegral a `shiftL` 8 .|. fromIntegral b
    _ -> lift empty

expect :: (MonadIO m) => Word8 -> ConduitM (Maybe Word8) o (MaybeT m) ()
expect n =
  await >>= \case
    Just (Just x)
      | x == n -> pure ()
      | otherwise -> liftIO (sayErr ("expected " <> show' n <> " got " <> show' x))
    _ -> lift empty

show' = T.pack . show

parseTag :: Monad m => ConduitM (Maybe Word8) o m (Maybe Tag)
parseTag =
  await >>= \case
    Nothing -> pure Nothing
    Just Nothing -> parseTag
    Just (Just tag) -> pure (Just (Tag (ShowHex tag)))

-- parse7584 :: Monad m => ConduitM (Maybe Word16) o m (Maybe Packet)
-- parse7584 = runMaybeC $
--   Type7584 <$> get <*> get <*> get <*> get <*> get

-- parse49344 :: Monad m => ConduitM (Maybe Word16) o m (Maybe Packet)
-- parse49344 = runMaybeC $
--   Type49344 <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get

get :: Monad m => ConduitM (Maybe Word16) o (MaybeT m) Word16
get = await >>= \case
  Nothing -> lift empty
  Just Nothing -> lift empty
  Just (Just x) -> pure x

awaitN :: Monad m => Natural -> ConduitM i o m [i]
awaitN = \case
  0 -> pure []
  n -> await >>= \case
    Nothing -> pure []
    Just x -> (x:) <$> awaitN (pred n)

-- | Parse until the next blank
parseUnknownPayload :: Monad m => ConduitM (Maybe Word8) o m Payload
parseUnknownPayload = Payload . fmap ShowHex <$> go mempty
  where
    go s =
      await >>= \case
        Nothing -> pure s
        Just Nothing -> pure s
        Just (Just w) -> go (s `V.snoc` w)

isC :: Packet -> Bool
isC = \case
  (TypeC {}) -> True
  _ -> False

isUnknown :: Packet -> Bool
isUnknown = \case
  (Unknown {}) -> True
  _ -> False

isA0 :: Packet -> Bool
isA0 (Unknown (Payload p)) = p V.! 1 == 0xa0

myFilter :: V.Vector Packet -> V.Vector Packet
myFilter = V.filter (none [isC, isA0])

none :: [a -> Bool] -> a -> Bool
none fs x = not $ any ($x) fs

graph3 :: V.Vector Parse.Packet -> IO ()
graph3 =
  let fs =
        [ \(Parse.Type3 x _ _ _ _ _) -> realToFrac x
        , \(Parse.Type3 _ x _ _ _ _) -> realToFrac x
        , \(Parse.Type3 _ _ x _ _ _) -> realToFrac x
        , \(Parse.Type3 _ _ _ x _ _) -> realToFrac x
        , \(Parse.Type3 _ _ _ _ x _) -> realToFrac x
        , \(Parse.Type3 _ _ _ _ _ x) -> realToFrac x
        ]
  in graphAll "03-" fs . V.filter Parse.is3

graph5 :: V.Vector Parse.Packet -> IO ()
graph5 =
  let fs =
        [ \(Parse.Type5 x _ _ _ _ _) -> realToFrac x
        , \(Parse.Type5 _ x _ _ _ _) -> realToFrac x
        , \(Parse.Type5 _ _ x _ _ _) -> realToFrac x
        , \(Parse.Type5 _ _ _ x _ _) -> realToFrac x
        , \(Parse.Type5 _ _ _ _ x _) -> realToFrac x
        , \(Parse.Type5 _ _ _ _ _ x) -> realToFrac x
        ]
  in graphAll "05-" fs . V.filter Parse.is5

graphV :: V.Vector Parse.Packet -> IO ()
graphV =
  let fs =
        [ \(Parse.TypeV x _ _ _ _ _) -> realToFrac x
        , \(Parse.TypeV _ x _ _ _ _) -> realToFrac x
        , \(Parse.TypeV _ _ x _ _ _) -> realToFrac x
        , \(Parse.TypeV _ _ _ x _ _) -> realToFrac x
        , \(Parse.TypeV _ _ _ _ x _) -> realToFrac x
        , \(Parse.TypeV _ _ _ _ _ x) -> realToFrac x
        ]
  in graphAll "0V-" fs . V.filter Parse.isV

-- graph16 :: V.Vector Parse.Packet -> IO ()
-- graph16 =
--   let fs =
--         [ \(Parse.Type16 x _ _ _ _ _) -> realToFrac x
--         , \(Parse.Type16 _ x _ _ _ _) -> realToFrac x
--         , \(Parse.Type16 _ _ x _ _ _) -> realToFrac x
--         , \(Parse.Type16 _ _ _ x _ _) -> realToFrac x
--         , \(Parse.Type16 _ _ _ _ x _) -> realToFrac x
--         , \(Parse.Type16 _ _ _ _ _ x) -> realToFrac x
--         ]
--   in graphAll "16-" fs . V.filter Parse.is16

graphFirstWord16 :: V.Vector Parse.Packet -> IO ()
graphFirstWord16 =
  let fs =
        [ \(Parse.Unknown v) ->
            realToFrac
              (((fromIntegral (v V.! 0) `shiftL` 8)
              .|. fromIntegral (v V.! 1) :: Integer) .&. 0x01ff)
        ]
  in graphAll "First-" fs . V.filter Parse.isUnknown

offsetsC :: V.Vector Packet -> [(Int16, Integer)]
offsetsC =
  fmap (\(a, b) -> (a, toInteger b - toInteger a)) .
  (zip <*> tail) . V.toList . fmap (\(TypeC _ _ _ _ x) -> x) . V.filter isC

offsets2C :: V.Vector Packet -> [Integer]
offsets2C ps =
  let os = snd <$> offsetsC ps
      ds = (zipWith (-) <*> tail) . (filter ((<10000) . abs)) $ os
  in ds

score :: FilePath -> IO Integer
score f = do
  ps <- parseFile f
  let is = offsets2C ps
  pure $ sum (abs <$> is)

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

-- reverseBits :: Word8 -> Word8
-- reverseBits i =
--   let b :: Word32
--       b = fromIntegral i
--       r = (((b * 0x0802 .&. 0x22110) .|. (b * 0x8020 .&. 0x88440)) * 0x10101) `shiftR` 16;
--   in fromIntegral r

--------------------------------------------------------------------------------
-- fiddling
--------------------------------------------------------------------------------

-- unknownLengths :: V.Vector Packet -> V.Vector (Tag, [(Natural, Natural)])
-- unknownLengths ps =
--   let ts = [(t, fromIntegral (V.length p)) | Unknown t (Payload p) <- V.toList ps]
--       ss = sortOn fst ts
--       gs = NE.groupWith fst ss
--       m :: NE.NonEmpty (Tag, Natural) -> (Tag, [(Natural, Natural)])
--       m l =
--         let ls = snd <$> l
--             ss = NE.sort ls
--             gs = NE.group ss
--         in (fst (NE.head l), fmap (NE.head &&& fromIntegral . NE.length) gs)
--   in V.fromList $ fmap m gs

