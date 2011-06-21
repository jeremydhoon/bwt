module TestBwt where

import qualified Data.Array as Array
import qualified Data.Array.Unboxed as UArray
import qualified Data.Array.ST as ST
import qualified Data.Binary as Binary
import qualified Data.Bits as Bits
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Word as Word
import qualified Test.QuickCheck as Qc
import qualified Debug.Trace as Trace

import qualified Bitlist
import qualified Bwt
import qualified HuffTree as Huff
import qualified UnicodeMtf as Mtf

instance Qc.Arbitrary Char where
  arbitrary = Qc.choose ('a', 'c') -- ('\0', '\255')

instance Qc.Arbitrary Word.Word8 where
  arbitrary = do
    i <- Qc.choose (0, 255) --(Char.ord 'a', Char.ord 'c')
    return $ fromIntegral (i :: Int)

invertRotstr :: String -> Bool
invertRotstr s =
  s == (Bwt.stringFromRotstr $ (buildRotstrZero . Bwt.buildFixstr) s)
  where
    buildRotstrZero fs = Bwt.buildRotstr fs 0

invertBwt :: [Word.Word8] -> Bool
invertBwt s = s == dec
  where
    (brk,s') = Bwt.bwtFast s
    dec = Bwt.reverseBwtFast s' brk

shiftVectorsMatch :: [Word.Word8] -> Bool
shiftVectorsMatch s = s == result
  where
    t = Bwt.buildBwtShiftVectorFast s
    f = List.sort s
    recover = (f !!)
    result = map recover $ UArray.elems t

invertRle :: [Word.Word8] -> Bool
invertRle s = s == (Bwt.runLengthDecode $ Bwt.runLengthEncode s)

invertExpandedRle :: [Word.Word8] -> Bool
invertExpandedRle s =
  s == (Bwt.runLengthDecodePairs $ Bwt.expandRlList $ Bwt.runLengthEncode s)

invertWordPacking :: [Bool] -> Bool
invertWordPacking fs = --Trace.trace ((show fs') ++ ", " ++ (show unpacked)) $
  check fs' unpacked
  where
    fs' = take 8 fs
    (wrd,_) = Bitlist.packWord fs'
    unpacked = Bitlist.trimToLength (length fs') $ Bitlist.unpackWord [] wrd
    none [] = True
    none (hd:rest) = if not hd then none rest else False
    check [] unpk = none unpk
    check (hdOrig:restOrig) (hdUnpk:restUnpk) =
      if hdOrig == hdUnpk then check restOrig restUnpk else False
    check orig [] = False    

invertPacking :: [Bool] -> Bool
invertPacking fs = --Trace.trace ((show fs) ++ ", " ++ (show unpacked)) $
  fs == unpacked
  where
    unpacked = Bitlist.unpackBits (Bitlist.packBits fs)

invertMtf :: [Word.Word8] -> Bool
invertMtf s = s == dec
  where
    enc = Mtf.mtf s
    dec = Mtf.reverseMtf enc

invertHuff :: [Word.Word8] -> Bool
invertHuff sRaw = s == dec
  where
    s = 0:1:sRaw
    (tree,bitlist) = Huff.huffencode s
    dec = Huff.huffdecode tree bitlist

invertEncoding :: [Word.Word8] -> Bool
invertEncoding sRaw = s == dec
  where
    s = 0:sRaw
    enc = Bwt.encode s
    dec = Bwt.decode enc

qsortOrder :: [Int] -> Bool
qsortOrder xs = (List.sort xs) == sorted
  where
    sorted = Bwt.sortListInPlace compare xs

main :: IO ()
main = do
  Qc.quickCheck invertBwt
  Qc.quickCheck invertRotstr
  Qc.quickCheck invertBwt
  Qc.quickCheck shiftVectorsMatch
  Qc.quickCheck invertRle
  Qc.quickCheck invertExpandedRle
  Qc.quickCheck invertWordPacking
  Qc.quickCheck invertPacking
  Qc.quickCheck invertMtf
  Qc.quickCheck invertHuff
  Qc.quickCheck invertEncoding
  Qc.quickCheck qsortOrder
