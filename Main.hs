module Main where

import qualified Data.Binary as Binary

import Bwt

blocksize :: Int
blocksize = 800000

main :: IO ()
main = do
  contents <- readFile "book1"
  let enc = Bwt.encode $ take blocksize contents
  
  Binary.encodeFile "out.bz" enc
  putStrLn "encoding complete..."
  unbin <- Binary.decodeFile "out.bz"
  putStrLn $ Bwt.decode unbin