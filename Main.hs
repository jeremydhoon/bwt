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

data Option = OptEncode | OptDecode | OptBlocksize String

options :: [GetOpt.OptDescr Option]
options = [GetOpt.Option ['c'] ["encode"] (GetOpt.NoArg OptEncode)
             "encode input",
           GetOpt.Option ['x'] ["decode"] (GetOpt.NoArg OptDecode)
             "decode input",
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
    check (Just OptEncode) (OptEncode:_) = Nothing
    check (Just OptDecode) (OptDecode:_) = Nothing
    check Nothing (OptEncode:rest) = check (Just OptEncode) rest
    check Nothing (OptDecode:rest) = check (Just OptDecode) rest
    check found (_:rest) = check found rest

extractBlocksize :: [Option] -> Int
extractBlocksize [] = defaultBlocksize
extractBlocksize ((OptBlocksize si):_)= read si
extractBlocksize (_:rest) = extractBlocksize rest

data Result = EncodeResult [Bwt.CodedBlock] | DecodeResult [Word.Word8]
  | Error String

main :: IO ()
main = do
  argv <- System.getArgs
  result <- case GetOpt.getOpt GetOpt.Permute options argv of
    (opts,[],[]) -> do
      let blocksize = extractBlocksize opts
      let encdir = extractEncodingDirection opts
      input <- ByteString.getContents
      case encdir of
        Just OptEncode -> do
          let bytes = ByteString.unpack input
          return $ EncodeResult $ compressBlocks bytes blocksize
        Just OptDecode ->
          return $ DecodeResult $ decodeBlocks $ Binary.decode input
        Nothing -> return $ Error "only one of 'x' and 'c' may be specified."
          
    (_,_,errors) -> do
      mapM putStrLn errors
      return $ Error "Invalid arguments."
  case result of
    EncodeResult chunks -> ByteString.putStr $ Binary.encode chunks
    DecodeResult cs -> ByteString.putStr $ ByteString.pack cs
    Error s -> IO.hPutStrLn IO.stderr s