{-# Language ScopedTypeVariables #-}
module Data.Blobster.BasicSpec (main, spec) where

import Data.Blobster

import Control.Exception
import Control.Monad
import System.IO.Temp

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

withTempDb  fn =
  withSystemTempDirectory "blobster-spec" $ \s -> makeDir s >>= fn

spec :: Spec
spec = around withTempDb $ do

  describe "basic" $ do
    it "Writes a number to blobster" $ \db -> do
      bid <- putBlob db (1 :: Int)
      n <- getBlob db bid :: IO (Either String Int)
      n `shouldBe` (Right 1)

  describe "basic" $ do
      it "Writes a lots of numbers to blobster" $ \db -> do
        replicateM_ 100 $ do
--             property $ \x -> do
              bid <- putBlob db (1 :: Int)
              n <- getBlob db bid :: IO (Either String Int)
              n `shouldBe` (Right 1)

main :: IO ()
main = hspec spec
