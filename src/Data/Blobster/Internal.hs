{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
module Data.Blobster.Internal where

import Control.Applicative
import Control.Monad
import Crypto.Hash
import Data.ByteString (ByteString)
import Data.Data
import Data.List (splitAt)
import Data.Maybe
import Data.SafeCopy
import Data.Serialize (Serialize)
import GHC.Generics
import System.Directory
import System.FilePath
import System.FilePath.Posix

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Serialize.Get as S
import qualified Data.Serialize.Put as S

class InnerPath a where
  innerPath :: a -> (FilePath, FilePath)

newtype BlobID = BlobID { blobID :: ByteString }
                 deriving (Eq,Ord,Generic)

newtype ObjectID = ObjectID { objectID :: ByteString }
                   deriving (Eq,Ord,Data,Generic)

newtype ObjectRef = ObjectRef BlobID
                    deriving (Eq,Ord,Generic)

deriveSafeCopy 1 'base ''BlobID
deriveSafeCopy 1 'base ''ObjectID
deriveSafeCopy 1 'base ''ObjectRef

instance Show BlobID where
  show bid = mconcat ["BlobID ", "\"", p, ps, "\""]
    where (p,ps) = innerPath bid

instance Show ObjectID where
  show oid = mconcat ["ObjectID ", "\"", p, ps, "\""]
    where (p,ps) = innerPath oid

instance Show ObjectRef where
  show oid = mconcat ["ObjectRef ", "\"", p, ps, "\""]
    where (p,ps) = innerPath oid

instance InnerPath BlobID where
  innerPath (BlobID b) = splitAt 2 b16
    where b16 = BS8.unpack (Base16.encode b)

instance InnerPath ObjectID where
  innerPath (ObjectID b) = splitAt 2 b16
    where b16 = BS8.unpack (Base16.encode b)

instance InnerPath ObjectRef where
  innerPath (ObjectRef b) = innerPath b

data Blobster = Blobster { prefixPath :: FilePath
                         , prefixBlob :: FilePath
                         , prefixRef  :: FilePath
                         } deriving (Show)

defBlobster :: FilePath -> Blobster
defBlobster p = Blobster { prefixPath = p
                         , prefixBlob = p </> "blob"
                         , prefixRef  = p </> "ref"
                         }

makeDir :: FilePath -> IO Blobster
makeDir pref = do
  let conf = defBlobster pref
  createDirectoryIfMissing True (prefixBlob conf)
  createDirectoryIfMissing True (prefixRef conf)
  return conf

putObject :: SafeCopy a => Blobster -> ObjectID -> a -> IO BlobID
putObject cfg oid o = do
  -- FIXME: exception handling
  bid <- putBlob cfg o
  _   <- putBlob' cfg True (prefixRef cfg) oid (S.runPut $ safePut (ObjectRef bid))
  return bid

getObject :: SafeCopy a => Blobster -> ObjectID -> IO (Either String a)
getObject cfg oid = do
  oref <- getBlob' cfg (prefixRef cfg) oid :: IO (Either String ObjectRef)
  case oref of
    Left s -> return $ Left s
    Right (ObjectRef bid) -> getBlob cfg bid

putBlob :: SafeCopy a => Blobster -> a -> IO BlobID
putBlob cfg x = do
  let blob = S.runPut $ safePut x
  let blobid = makeBlobID blob
  putBlob' cfg False (prefixBlob cfg) blobid blob
  return blobid

getBlob :: SafeCopy a => Blobster -> BlobID -> IO (Either String a)
getBlob cfg = getBlob' cfg (prefixBlob cfg)

xferBlob :: InnerPath a => Bool -> Blobster -> Blobster -> a -> IO ()
xferBlob ow db1 db2 bid = do
  let from = blobPath db1 (prefixBlob db1) bid
  let to   = blobPath db2 (prefixBlob db2) bid

  write <- if ow
             then return True
             else not  <$> doesFileExist to

  when write $ do
    createDirectoryIfMissing True (takeDirectory to)
    copyFileWithMetadata from to

blobPath :: InnerPath a => Blobster -> FilePath -> a -> FilePath
{-# INLINE blobPath #-}
blobPath cfg pref bid = pref </> pp </> fname
  where (pp,fname) = innerPath bid

getBlob' :: (SafeCopy a, InnerPath oid)
         => Blobster
         -> FilePath
         -> oid
         -> IO (Either String a)

getBlob' cfg p oid = do
  let path = blobPath cfg p oid
  ex <- doesFileExist path
  if not ex
    then return (Left (mconcat ["not exists: ", path] ))
    else S.runGet safeGet <$> BS.readFile path

putBlob' :: InnerPath oid
         => Blobster
         -> Bool
         -> FilePath
         -> oid
         -> ByteString
         -> IO ()

putBlob' cfg ow pref oid blob = do
  let path = blobPath cfg pref oid
  createDirectoryIfMissing True (takeDirectory path)
  write <- if ow
             then return True
             else not <$> doesFileExist path
  when write $ BS.writeFile path blob

makeObjectID :: SafeCopy a => a -> ObjectID
makeObjectID o = ObjectID (hashKey (S.runPut $ safePut o))

makeBlobID :: ByteString -> BlobID
makeBlobID bs = BlobID (hashKey bs)

hashKey :: ByteString -> ByteString
hashKey bs = bytes
  where bytes = BS.pack (BA.unpack (hash bs :: Digest SHA1))
