{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeFamilies            #-}

module Data.Blobster ( BlobID(..)
                     , ObjectID(..)
                     , Blobster
                     , defBlobster
                     , makeDir
                     , getObject, putObject, getBlob, putBlob, xferBlob
                     , makeObjectID, makeBlobID
                     , Materialize(..)
                     , MaterializedEntry(..)
                     , materialize
                     , materialize'
                     , dematerialize
                     ) where

import Data.Blobster.Internal

import Control.Monad.IO.Class
import Data.Maybe
import Data.SafeCopy

data MaterializedEntry a = EntryIndexed (Maybe ObjectID) (Maybe a)
                         | EntryDirect  (Maybe BlobID)   (Maybe a)

class SafeCopy (ValueOf a) => Materialize a where
  type ValueOf a :: *

  toEntry   :: a -> MaterializedEntry (ValueOf a)
  fromEntry :: MaterializedEntry (ValueOf a) -> Maybe a

-- FIXME: rewrite boilerplate
materialize :: forall a m . (MonadIO m, Materialize a)
            => Blobster
            -> a
            -> m (Either String a)

materialize db x = do
  v <- materialize' db x
  case toEntry x of
    EntryIndexed i _ -> return (v >>= fromEntry' . EntryIndexed i . pure)
    EntryDirect i _  -> return (v >>= fromEntry' . EntryDirect i . pure)

  where
    fromEntry' a = maybe (Left "not representable") (Right) (fromEntry a)

materialize' :: forall a m . (MonadIO m, Materialize a)
            => Blobster
            -> a
            -> m (Either String (ValueOf a))
materialize' db x =
  case toEntry x of
    EntryIndexed (Just oid) Nothing -> do
      e <- (liftIO (getObject db oid :: IO (Either String (ValueOf a))))
      return e

    EntryIndexed i e@(Just v) -> return (Right v)

    EntryDirect (Just oid) Nothing -> do
      e <- (liftIO (getBlob db oid :: IO (Either String (ValueOf a))))
      return e

    EntryDirect i e@(Just v) -> return (Right v)

    EntryIndexed Nothing Nothing -> return $ Left "Entry key not specified"
    EntryDirect Nothing Nothing -> return $ Left "Entry key not specified"


dematerialize :: forall a m . (MonadIO m, Materialize a)
              => Blobster
              -> a
              -> m a
dematerialize db x = case toEntry x of
  EntryIndexed (Just oid) Nothing  -> return x

  EntryIndexed (Just oid) (Just v) -> do
    _ <- liftIO $ putObject db oid v
    return $ fromMaybe x (fromEntry (EntryIndexed (Just oid) Nothing))

  EntryIndexed Nothing _ -> return x -- FIXME: throw error ?!

  EntryDirect _ Nothing  -> return x
  EntryDirect _ (Just v) -> do
    bid <- liftIO $ putBlob db v
    return $ fromMaybe x (fromEntry (EntryDirect (Just bid) Nothing))

