{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Data.Blobster.MaterializeSpec (main, spec) where

import Data.Blobster

import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import Data.Data
import Data.Maybe (fromMaybe)
import Data.SafeCopy
import Data.String
import System.IO.Temp

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

-- FIXME: move to ???
withTempDb  fn =
  withSystemTempDirectory "blobster-spec-1" $ \s -> makeDir s >>= fn

makeTestBlobID :: SafeCopy a => a -> BlobID
makeTestBlobID = BlobID . objectID . makeObjectID

newtype OID = OID ByteString
              deriving (Eq,Ord,Show,Data)

newtype E1 = E1 (Either OID String)
             deriving (Eq,Ord,Show,Data)

data E2 = E2Ref ByteString | E2Value ByteString String
          deriving (Eq,Ord,Show,Data)

deriveSafeCopy 1 'base ''OID
deriveSafeCopy 1 'base ''E1

instance Materialize E1 where
  type ValueOf E1 = String

  toEntry (E1 (Right x)) = EntryDirect Nothing (Just x)
  toEntry (E1 (Left (OID i)))  = EntryDirect (Just (BlobID i)) Nothing

  fromEntry (EntryDirect _ (Just s))        = Just $ E1 (Right s)
  fromEntry (EntryDirect (Just i) Nothing)  = Just $ E1 (Left (OID (blobID i)))
  fromEntry (EntryDirect Nothing Nothing)   = Nothing

  fromEntry (EntryIndexed _  _)  = Nothing

instance Materialize E2 where
  type ValueOf E2 = String

  toEntry (E2Value oid s) = EntryIndexed (Just (ObjectID oid)) (Just s)
  toEntry (E2Ref oid)     = EntryIndexed (Just (ObjectID oid)) Nothing

  fromEntry (EntryDirect _ _) = Nothing
  fromEntry (EntryIndexed (Just i) Nothing) = Just $ E2Ref (objectID i)
  fromEntry (EntryIndexed (Just i) (Just o)) = Just $ E2Value (objectID i) o
  fromEntry _ = Nothing


spec :: Spec
spec = around withTempDb $ do
  describe "Materialize" $ do
    it "put / materialize test" $ \db -> do
      let a1 = E1 (Right "A1")
      bid <- putBlob db ("A1" :: String)
      s1 <- getBlob db bid :: IO (Either String String)
      let a2 = E1 (Left (OID (blobID bid)))
      a4 <- materialize db a2
      a4 `shouldBe` (Right a1)

    it "dematerialize / materialize test 1" $ \db -> do
      let a1 = E1 (Right "A2")
      a2 <- dematerialize db a1
      a3 <- materialize db a2
      Right a1 `shouldBe` a3

    it "dematerialize / materialize test 2" $ \db -> do
      let x1 = E2Value (objectID $ makeObjectID (123456789 :: Int)) "E2VALUE TEST TEST TEST"
      x2 <- dematerialize db x1
      x3 <- materialize db x2
      x3 `shouldBe` Right x1

main :: IO ()
main = hspec spec
