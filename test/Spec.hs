module Main where

import Data.Blobster

import Data.Either
import Data.ByteString (ByteString)
import Control.Monad
import System.IO.Temp
import Control.Concurrent

import Test.HUnit.Base

import qualified Data.ByteString.Char8 as BS


testBasic1 :: FilePath -> IO ()
testBasic1 dir = do

  putStrLn "testBasic1"

  cfg <- makeDir dir

  let keys =  fmap makeObjectID ['A'..'Z']

  let kv = zip keys ([1..]) :: [(ObjectID, Int)]

  mapM_ (uncurry (putObject cfg)) kv

  restored <- mapM (getObject cfg)  keys >>= return . rights :: IO [Int]

  assertEqual "seq-len-1" (length kv) (length restored)

  mapM_ (uncurry (assertEqual "")) (zip (fmap snd kv) restored)

  let j = "SOMERANDOMTESTSTRING" :: ByteString
  bid <- putBlob cfg j
  j' <- getBlob cfg bid :: IO (Either String ByteString)

  assertEqual "" (Right j) j'

testBasic2 :: FilePath -> IO ()
testBasic2 dir = do
  putStrLn "testBasic2"

  cfg <- makeDir dir

  let iis = [0..30000] :: [Int]

  bids <- forM iis $ \i -> do
    putBlob cfg i

  ee <- mapM (getBlob cfg) bids

  forM_ (zip (fmap Right iis) ee) $ \(e,r) ->
    assertEqual "" e r

  return ()

main :: IO ()
main = do

  putStrLn ""
  withSystemTempDirectory "blobtest" $ \dir -> do
    testBasic1 dir
    testBasic2 dir


