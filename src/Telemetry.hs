{-# LANGUAGE LambdaCase #-}

module Telemetry
  where

import           Conduit
import           Control.Applicative
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString.Char8     as BS8
import qualified Data.Text                 as T
import qualified Data.Vector               as V
import           Data.Word
import           Numeric.Natural

newtype Tag = Tag { unTag :: Word16 }
  deriving (Show, Eq, Ord)

newtype Payload = Payload { unPayload :: V.Vector Word16 }
  deriving (Show)

data Packet
  = Type7584 Word16 Word16 Word16 Word16 Word16
  | Type49344 Word16 Word16 Word16 Word16 Word16
                     Word16 Word16 Word16 Word16
  | Unknown Tag Payload
  | Error Tag Payload T.Text
  deriving (Show)

parseFile :: FilePath -> IO (V.Vector Packet)
parseFile file =
  runResourceT . runConduit $
  (sourceFile file =$= linesUnboundedAsciiC =$= mapC readBS =$= parse =$=
   sinkVector)

readBS :: Read a => BS8.ByteString -> a
readBS = read . BS8.unpack

parse :: Monad m => Conduit (Maybe Word16) m Packet
parse =
  parseTag >>= \case
    Nothing -> pure ()
    Just (Tag 7584) -> do
      parse7584 >>= \case
        Nothing -> pure ()
        Just p -> yield p
      parse
    Just (Tag 49344) -> do
      parse49344 >>= \case
        Nothing -> pure ()
        Just p -> yield p
      parse
    Just unknownTag -> do
      payload <- parseUnknownPayload
      yield (Unknown unknownTag payload)
      parse

parseTag :: Monad m => ConduitM (Maybe Word16) o m (Maybe Tag)
parseTag =
  await >>= \case
    Nothing -> pure Nothing
    Just Nothing -> parseTag
    Just (Just tag) -> pure (Just (Tag tag))

parse7584 :: Monad m => ConduitM (Maybe Word16) o m (Maybe Packet)
parse7584 = runMaybeC $
  Type7584 <$> get <*> get <*> get <*> get <*> get

parse49344 :: Monad m => ConduitM (Maybe Word16) o m (Maybe Packet)
parse49344 = runMaybeC $
  Type49344 <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get

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
parseUnknownPayload :: Monad m => ConduitM (Maybe Word16) o m Payload
parseUnknownPayload = Payload <$> go mempty
  where
    go s =
      await >>= \case
        Nothing -> pure s
        Just Nothing -> pure s
        Just (Just w) -> go (s `V.snoc` w)
