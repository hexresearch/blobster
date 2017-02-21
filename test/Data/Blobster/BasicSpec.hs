{-# Language ScopedTypeVariables #-}
{-# Language DeriveGeneric #-}
{-# Language OverloadedStrings #-}
module Data.Blobster.BasicSpec (main, spec) where

import Data.Blobster

import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.SafeCopy
import Data.Serialize
import Data.String
import GHC.Generics
import System.IO.Temp
import Text.Read (readMaybe)

import qualified Data.ByteString as BS

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

data CustomEnum = CE1 | CE2 | CE3
                  deriving (Show,Eq,Ord,Enum,Bounded,Generic)

data CustomDataExample = CustomDataExample { name :: String
                                           , age  :: Int
                                           , someData :: [Int]
                                           , someEnum :: [CustomEnum]
                                           } deriving (Show,Eq,Ord,Generic)

data NewCustomDataExample1 = NewCustomDataExample1 { new1Name     :: String
                                                   , new1Age      :: Int
                                                   , new1SomeData :: [Int]
                                                   , new1SomeEnum :: [CustomEnum]
                                                   , new1Field    :: String
                                                   } deriving (Show, Eq, Ord, Generic)

data NewCustomDataExample2 = NewCustomDataExample2 { new2Name     :: String
                                                   , new2Age      :: Int
                                                   , new2SomeData :: [Int]
                                                   , new2SomeEnum :: [CustomEnum]
                                                   , new2Field    :: Int
                                                   } deriving (Show, Eq, Ord, Generic)

instance Migrate NewCustomDataExample1 where
  type MigrateFrom NewCustomDataExample1 = CustomDataExample
  migrate CustomDataExample{..} = NewCustomDataExample1 {
      new1Name = name
    , new1Age  = age
    , new1SomeData = someData
    , new1SomeEnum = someEnum
    , new1Field = "42"
    }

instance Migrate NewCustomDataExample2 where
  type MigrateFrom NewCustomDataExample2 = NewCustomDataExample1
  migrate NewCustomDataExample1{..} = NewCustomDataExample2 {
      new2Name = new1Name
    , new2Age  = new1Age
    , new2SomeData = new1SomeData
    , new2SomeEnum = new1SomeEnum
    , new2Field = fromMaybe 0 $ readMaybe new1Field
    }

deriveSafeCopy 1 'base ''CustomEnum
deriveSafeCopy 1 'base ''CustomDataExample
deriveSafeCopy 2 'extension ''NewCustomDataExample1
deriveSafeCopy 3 'extension ''NewCustomDataExample2

instance Arbitrary ObjectID where
  arbitrary = makeObjectID <$> (BS.pack <$> arbitrary)

instance Arbitrary CustomEnum where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary CustomDataExample where
  arbitrary = CustomDataExample <$> arbitrary
                                <*> arbitrary
                                <*> arbitrary
                                <*> arbitrary

instance Arbitrary NewCustomDataExample1 where
  arbitrary = NewCustomDataExample1 <$> arbitrary
                                    <*> arbitrary
                                    <*> arbitrary
                                    <*> arbitrary
                                    <*> arbitrary

withTempDb  fn =
  withSystemTempDirectory "blobster-spec" $ \s -> makeDir s >>= fn

spec :: Spec
spec = around withTempDb $ do
  describe "basic" $ do
    it "Writes/reads a number to blobster" $ \db -> do
      bid <- putBlob db (1 :: Int)
      n <- getBlob db bid :: IO (Either String Int)
      n `shouldBe` Right 1

    it "Writes/reads a lots of numbers to blobster" $ \db -> do
      replicateM_ 1000 $ do
        ints <- generate arbitrary :: IO [Int]
        bids <- mapM (putBlob db) ints
        eints <- mapM (getBlob db) bids
        eints `shouldBe` fmap Right ints

    it "Writes/reads a lots of complex objects" $ \db -> do
      replicateM_ 1000 $ do
        c <- generate arbitrary :: IO CustomDataExample
        bid <- putBlob db c
        c1 <- getBlob db bid
        c1 `shouldBe` Right c

    it "Writes/reads a single indexed object" $ \db -> do
      oid <- generate arbitrary :: IO ObjectID
      o <- generate arbitrary :: IO CustomDataExample
      putObject db oid o
      o' <- getObject db oid :: IO (Either String CustomDataExample)
      o' `shouldBe` Right o

    it "Writes/reads a lot of indexed objects" $ \db -> do
      replicateM_ 100 $ do
        oid <- generate arbitrary :: IO ObjectID
        o <- generate arbitrary :: IO CustomDataExample
        putObject db oid o
        o' <- getObject db oid :: IO (Either String CustomDataExample)
        o' `shouldBe` Right o

    it "Creates objects in the first db and moves them to the new db" $ \db -> do
      a <- replicateM 1000 $ do
        c <- generate arbitrary :: IO CustomDataExample
        bid <- putBlob db c
        return (bid,c)

      withTempDb $ \db1 -> do
        forM_ a $ \(b,_) -> do
          xferBlob False db db1 b

        o2 <- mapM (getBlob db1 . fst) a
        let o1 = fmap (Right . snd) a

        o2 `shouldBe` o1

  describe "migration" $ do
    it "Migrates from basic format" $ \db ->
      replicateM_ 100 $ do
        oid <- generate arbitrary :: IO ObjectID
        o <- generate arbitrary :: IO CustomDataExample
        putObject db oid o
        o' <- getObject db oid :: IO (Either String NewCustomDataExample1)
        o' `shouldBe` Right (migrate o)

    it "Migrates from extended format" $ \db ->
      replicateM_ 100 $ do
        oid <- generate arbitrary :: IO ObjectID
        o <- generate arbitrary :: IO NewCustomDataExample1
        putObject db oid o
        o' <- getObject db oid :: IO (Either String NewCustomDataExample2)
        o' `shouldBe` Right (migrate o)

    it "Migrates from base to last format" $ \db ->
      replicateM_ 100 $ do
        oid <- generate arbitrary :: IO ObjectID
        o <- generate arbitrary :: IO CustomDataExample
        putObject db oid o
        o' <- getObject db oid :: IO (Either String NewCustomDataExample2)
        o' `shouldBe` Right (migrate $ migrate o)

main :: IO ()
main = hspec spec
