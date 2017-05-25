{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLists            #-}

module Telemetry
  where

import           Conduit
import           Control.Applicative
import           Control.Arrow             ((&&&))
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Data.Bits
import qualified Data.ByteString.Char8     as BS8
import           Data.Foldable
import           Data.Int
import           Data.List.Extra           (chunksOf, genericLength, sortOn)
import qualified Data.List.NonEmpty        as NE
import           Data.Maybe
import qualified Data.Text                 as T
import qualified Data.Vector               as V
import           Data.Word
import           Graph
import           Numeric
import           Numeric.Natural

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
  = Unknown Payload
  | Error Tag Payload T.Text
  deriving (Show)

parseFile :: FilePath -> IO (V.Vector Packet)
parseFile file =
  runResourceT . runConduit $
  (sourceFile file =$= linesUnboundedAsciiC =$= mapC readBS =$= splitBytes =$= parse =$=
   sinkVector)

readBS :: Read a => BS8.ByteString -> a
readBS = read . BS8.unpack

splitBytes :: Monad m => Conduit (Maybe Word16) m (Maybe Word8)
splitBytes = concatMapC $ \case
  Nothing -> [Nothing] :: [Maybe Word8]
  Just x -> [Just (fromIntegral (x `shiftR` 8)), Just (fromIntegral x)]

parse :: Monad m => Conduit (Maybe Word8) m Packet
parse = do
  dropWhileC isNothing
  p <- parseUnknownPayload
  unless (V.null (unPayload p)) $ do
    yield (Unknown p)
    parse

  -- parseTag >>= \case
  --   Nothing -> pure ()
  --   -- Just (Tag 7584) -> do
  --   --   parse7584 >>= \case
  --   --     Nothing -> pure ()
  --   --     Just p -> yield p
  --   --   parse
  --   -- Just (Tag 49344) -> do
  --   --   parse49344 >>= \case
  --   --     Nothing -> pure ()
  --   --     Just p -> yield p
  --   --   parse
  --   Just unknownTag -> do
  --     payload <- parseUnknownPayload
  --     yield (Unknown unknownTag payload)
  --     parse

-- parseTag :: Monad m => ConduitM (Maybe Word16) o m (Maybe Tag)
-- parseTag =
--   await >>= \case
--     Nothing -> pure Nothing
--     Just Nothing -> parseTag
--     Just (Just tag) -> pure (Just (Tag (ShowHex (tag .&. 0xff))))

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

isCE :: Packet -> Bool
isCE (Unknown (Payload p)) = V.take 2 p == [0xc, 0xe]

isA0 :: Packet -> Bool
isA0 (Unknown (Payload p)) = p V.! 1 == 0xa0

myFilter :: V.Vector Packet -> V.Vector Packet
myFilter = V.filter (none [isCE, isA0])

none :: [a -> Bool] -> a -> Bool
none fs x = not $ any ($x) fs

graphCE :: V.Vector Packet -> IO ()
graphCE =
  graphAll . fmap (fmap unGap) . fmap unSplit . fmap (\(Unknown (Payload p)) -> fmap unShowHex p) . V.filter isCE

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

unGap = unGap64 . unGap1024

unGap1024 :: Word16 -> Word16
unGap1024 x =
  let d = x `div` 1024
  in x - 414 * d

unGap64 :: Word16 -> Word16
unGap64 x =
  let d = x `div` 64
  in x - 26 * d

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

