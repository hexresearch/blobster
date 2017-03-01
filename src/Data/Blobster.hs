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

materialize :: forall a m . (MonadIO m, Materialize a)
            => Blobster
            -> a
            -> m (Either String a)

materialize db x =
  case toEntry x of
    EntryIndexed (Just oid) Nothing -> do
      e <- (liftIO (getObject db oid :: IO (Either String (ValueOf a))))
      return $  e >>= fromEntry' . EntryIndexed (Just oid) . Just

    EntryIndexed i e@(Just _) -> return $ fromEntry' (EntryIndexed i e)

    EntryDirect (Just oid) Nothing -> do
      e <- (liftIO (getBlob db oid :: IO (Either String (ValueOf a))))
      return $ e >>= fromEntry' . EntryDirect (Just oid) . Just

    EntryDirect i e@(Just _) -> return $ fromEntry' (EntryDirect i e)

    EntryIndexed Nothing Nothing -> return $ Left "Entry key not specified"
    EntryDirect Nothing Nothing -> return $ Left "Entry key not specified"


  where
    fromEntry' a = maybe (Left "not representable") (Right) (fromEntry a)


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

