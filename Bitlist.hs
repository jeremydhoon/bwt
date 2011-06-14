module Bitlist where

import qualified Data.Bits as Bits
import qualified Data.Word as Word

type Bitlist = ([Word.Word8], Int)

empty :: Bitlist
empty = ([], 0)

size :: Bitlist -> Int
size (_,n) = n

null :: Bitlist -> Bool
null bl = (size bl) == 0

trimToLength :: Int -> [a] -> [a]
trimToLength n xs = drop ((length xs) - n) xs

bitify :: Bool -> Word.Word8
bitify f = (if f then 1 else 0) :: Word.Word8

packWord :: [Bool] -> (Word.Word8, [Bool])
packWord fs = (out, drop 8 fs)
  where
    fs' = take 8 fs
    lshift wrd = wrd `Bits.shiftL` 1
    out = foldl (\acc f -> (lshift acc) + (bitify f)) (0 :: Word.Word8) fs'

packBits :: [Bool] -> Bitlist
packBits fs = (handleWord [] fs, length fs)
  where
    handleWord ret [] = reverse ret
    handleWord ret rest = handleWord (wrd:ret) nextRest
      where
        (wrd,nextRest) = packWord rest

revAppend :: [a] -> [a] -> [a]
revAppend acc [] = acc
revAppend acc (x:xs) = revAppend (x:acc) xs

unpackWord :: [Bool] -> Word.Word8 -> [Bool]        
unpackWord acc wrd = handleBit acc 8 wrd
  where
    rshift wrd = wrd `Bits.shiftR` 1
    boolify wrd = (wrd `mod` 2) > 0
    handleBit ret 0 wrd = ret
    handleBit ret count wrd =
      handleBit ((boolify wrd):ret) (count - 1) (rshift wrd)

unpackBits :: Bitlist -> [Bool]
unpackBits (wrds, nbits) =
  if (nbits `mod` 8) /= 0 then unpacked else alt
  where
    (start,last) = splitAt ((length wrds) - 1) $ wrds
    front = foldl unpackWord [] $ reverse start
    lastWordBits = unpackWord [] (head last)
    back = trimToLength (nbits `mod` 8) lastWordBits
    unpacked = front ++ back
    alt = foldl unpackWord [] $ reverse wrds