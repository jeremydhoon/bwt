module Main where

import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.List as List
import qualified Data.Word as Word
import qualified System
import qualified System.Console.GetOpt as GetOpt
import qualified System.IO as IO

import qualified Bwt
import qualified Common

defaultBlocksize :: Int
defaultBlocksize = 800000

defaultEncodingDirection :: Option
defaultEncodingDirection = OptEncode

data Option = OptEncode | OptDecode | OptBwt | OptBlocksize String deriving Eq

options :: [GetOpt.OptDescr Option]
options = [GetOpt.Option ['c'] ["encode"] (GetOpt.NoArg OptEncode)
             "encode input",
           GetOpt.Option ['x'] ["decode"] (GetOpt.NoArg OptDecode)
             "decode input",
           GetOpt.Option ['w'] ["bwt"] (GetOpt.NoArg OptBwt)
             "bwt transform input",
           GetOpt.Option ['b'] ["blocksize"]
             (GetOpt.ReqArg OptBlocksize (show defaultBlocksize))
             "characters per block"]

nextBlock :: Int -> [Bwt.CodedBlock] -> [Word.Word8] -> [Bwt.CodedBlock]
nextBlock blocksize acc s = case blockS of
  [] -> reverse acc
  bs -> nextBlock blocksize ((Bwt.encode bs):acc) s'
  where
    (blockS, s') = List.splitAt blocksize s

compressBlocks :: [Word.Word8] -> Int -> [Bwt.CodedBlock]
compressBlocks s blocksize = nextBlock blocksize [] s

decodeBlocks :: [Bwt.CodedBlock] -> [Word.Word8]
decodeBlocks = reverse . (List.foldl' Common.revAppend []) . (map Bwt.decode)

extractEncodingDirection :: [Option] -> Maybe Option
extractEncodingDirection opts = check Nothing opts
  where
    check Nothing [] = Just defaultEncodingDirection
    check found@(Just _) [] = found
    check found@(Just opt) (hd:rest) =
      if opt == hd then check found rest
      else Nothing
    check Nothing (hd:rest) = check found rest
      where
        found = case hd of
          OptEncode -> Just OptEncode
          OptDecode -> Just OptDecode
          OptBwt -> Just OptBwt
          _ -> Nothing

extractBlocksize :: [Option] -> Int
extractBlocksize [] = defaultBlocksize
extractBlocksize ((OptBlocksize si):_)= read si
extractBlocksize (_:rest) = extractBlocksize rest

data Result = EncodeResult [Bwt.CodedBlock] | DecodeResult [Word.Word8]
  | Error String | BwtResult (Int, [Word.Word8])

main :: IO ()
main = do
  argv <- System.getArgs
  result <- case GetOpt.getOpt GetOpt.Permute options argv of
    (opts,[],[]) -> do
      let blocksize = extractBlocksize opts
      let encdir = extractEncodingDirection opts
      input <- ByteString.getContents
      let bytes = ByteString.unpack input
      case encdir of
        Just OptEncode -> do
          return $ EncodeResult $ compressBlocks bytes blocksize
        Just OptDecode ->
          return $ DecodeResult $ decodeBlocks $ Binary.decode input
        Just OptBwt ->
          return $ BwtResult $ Bwt.bwtFast bytes
        Nothing -> return $ Error "only one of 'x' and 'c' may be specified."
          
    (_,_,errors) -> do
      mapM putStrLn errors
      return $ Error "Invalid arguments."
  case result of
    EncodeResult chunks -> ByteString.putStr $ Binary.encode chunks
    DecodeResult cs -> ByteString.putStr $ ByteString.pack cs
    BwtResult bs -> ByteString.putStr $ Binary.encode bs
    Error s -> IO.hPutStrLn IO.stderr s