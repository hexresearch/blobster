{-# Language ScopedTypeVariables #-}
{-# Language DeriveGeneric #-}
module Data.Blobster.BasicSpec (main, spec) where

import Data.Blobster

import Control.Exception
import Control.Monad
import System.IO.Temp
import Data.Serialize
import GHC.Generics

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


instance Serialize CustomDataExample
instance Serialize CustomEnum

instance Arbitrary CustomEnum where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary CustomDataExample where
  arbitrary = CustomDataExample <$> arbitrary
                                <*> arbitrary
                                <*> arbitrary
                                <*> arbitrary

withTempDb  fn =
  withSystemTempDirectory "blobster-spec" $ \s -> makeDir s >>= fn

spec :: Spec
spec = around withTempDb $ do

  describe "basic-1" $ do
    it "Writes/reads a number to blobster" $ \db -> do
      bid <- putBlob db (1 :: Int)
      n <- getBlob db bid :: IO (Either String Int)
      n `shouldBe` (Right 1)

  describe "basic-2" $ do
      it "Writes/reads a lots of numbers to blobster" $ \db -> do
        replicateM_ 1000 $ do
              ints <- generate arbitrary :: IO [Int]
              bids <- mapM (putBlob db) ints
              eints <- mapM (getBlob db) bids
              (fmap Right ints)  `shouldBe` eints

  describe "basic-3" $ do
      it "Writes/reads a lots of complex objects" $ \db -> do
        replicateM_ 1000 $ do
              c <- generate arbitrary :: IO CustomDataExample
              bid <- putBlob db c
              c1 <- getBlob db bid
              c1 `shouldBe` (Right c)

main :: IO ()
main = hspec spec
