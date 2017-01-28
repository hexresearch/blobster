{-# Language DeriveGeneric #-}
module Data.Blobster ( BlobID
                     , ObjectID(..)
                     , Blobster
                     , defBlobster
                     , makeDir
                     , getObject, putObject, getBlob, putBlob
                     , makeObjectID, makeBlobID
                     ) where

import Control.Applicative
import Control.Monad
import Crypto.Hash
import Data.ByteString (ByteString)
import Data.List (splitAt)
import Data.Maybe
import Data.Serialize (Serialize)
import GHC.Generics
import System.Directory
import System.FilePath
import System.FilePath.Posix

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Serialize as S

class InnerPath a where
  innerPath :: a -> (FilePath, FilePath)

newtype BlobID = BlobID ByteString
                 deriving (Eq,Ord,Generic)

newtype ObjectID = ObjectID ByteString
                   deriving (Eq,Ord,Generic)

newtype ObjectRef = ObjectRef BlobID
                    deriving (Show,Eq,Ord,Generic)

instance Serialize BlobID
instance Serialize ObjectID
instance Serialize ObjectRef

instance Show BlobID where
  show bid = mconcat ["BlobID ", "\"", p, ps, "\""]
    where (p,ps) = innerPath bid

instance Show ObjectID where
  show oid = mconcat ["ObjectID", "\"", p, ps, "\""]
    where (p,ps) = innerPath oid

instance InnerPath BlobID where
  innerPath (BlobID b) = splitAt 2 b16
    where b16 = BS8.unpack (Base16.encode b)

instance InnerPath ObjectID where
  innerPath (ObjectID b) = splitAt 2 b16
    where b16 = BS8.unpack (Base16.encode b)

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

putObject :: Serialize a => Blobster -> ObjectID -> a -> IO BlobID
putObject cfg oid o = do
  -- FIXME: exception handling
  bid <- putBlob cfg o
  _   <- putBlob' cfg True (prefixRef cfg) oid (S.encode (ObjectRef bid))
  return bid

getObject :: Serialize a => Blobster -> ObjectID -> IO (Either String a)
getObject cfg oid = do
  oref <- getBlob' cfg (prefixRef cfg) oid :: IO (Either String ObjectRef)
  case oref of
    Left s -> return $ Left s
    Right (ObjectRef bid) -> getBlob cfg bid

putBlob :: Serialize a => Blobster -> a -> IO BlobID
putBlob cfg x = do
  let blob = S.encode x
  let blobid = makeBlobID blob
  putBlob' cfg False (prefixBlob cfg) blobid blob
  return blobid

getBlob :: Serialize a => Blobster -> BlobID -> IO (Either String a)
getBlob cfg bid = getBlob' cfg (prefixBlob cfg) bid

getBlob' :: (Serialize a, InnerPath oid)
         => Blobster
         -> FilePath
         -> oid
         -> IO (Either String a)

getBlob' cfg p oid = do
  let (pp,fname) = innerPath oid
  let path = p </> pp </> fname
  ex <- doesFileExist path
  case ex of
    False -> return (Left (mconcat ["not exists: ", path] ))
    True  -> BS.readFile path >>= return . S.decode

putBlob' :: InnerPath oid
         => Blobster
         -> Bool
         -> FilePath
         -> oid
         -> ByteString
         -> IO ()

putBlob' cfg ow pref oid blob = do
  let (p,fname) = innerPath oid
  let pp = pref </> p
  let path = pp </> fname
  createDirectoryIfMissing True pp
  write <- if ow
             then return True
             else not <$> doesFileExist path

  when write $ BS.writeFile path blob

makeObjectID :: Serialize a => a -> ObjectID
makeObjectID o = ObjectID (hashKey (S.encode o))

makeBlobID :: ByteString -> BlobID
makeBlobID bs = BlobID (hashKey bs)

hashKey :: ByteString -> ByteString
hashKey bs = bytes
  where bytes = BS.pack (BA.unpack (hash bs :: Digest SHA1))

