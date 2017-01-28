module Main where

import Data.Blobster

import Data.Either
import Data.ByteString (ByteString)
import Control.Monad

import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
  cfg <- makeDir "db"
  print cfg

  let keys =  fmap makeObjectID ['A'..'Z']

  let kv = zip keys ([1..]) :: [(ObjectID, Int)]

  mapM_ (uncurry (putObject cfg)) kv

  restored <- mapM (getObject cfg)  keys >>= return . rights :: IO [Int]

  mapM_ print (zip (fmap snd kv) restored)

  let j = "JOPAKITA" :: ByteString
  bid <- putBlob cfg j
  print bid
  j' <- getBlob cfg bid :: IO (Either String ByteString)

  print (j, j')

  print "wut"
