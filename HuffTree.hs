module HuffTree where

import qualified Control.Monad as Monad

import qualified Data.Binary as Binary
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Word as Word
import qualified Debug.Trace as Trace

import qualified Bitlist
import qualified Common
import qualified MultiHeap

data HuffTree a = HuffLeaf a | HuffNode (HuffTree a) (HuffTree a)

inccount :: Ord a => a -> Map.Map a Int -> Map.Map a Int
inccount x m = Map.insert x ((Map.findWithDefault 0 x m) + 1) m

freqs :: Ord a => [a] -> MultiHeap.MultiHeap Int a
freqs xs = foldl (\mh (c,i) -> MultiHeap.insert i c mh) MultiHeap.empty pairs
  where
    pairs = Map.assocs $ foldl (\m x -> inccount x m) Map.empty xs

buildTree :: Ord a => [a] -> HuffTree a
buildTree xs = combine huffHeap
  where
    charHeap = freqs xs
    huffHeap = MultiHeap.fromList $ map (\(k,v) -> (k, HuffLeaf v)) $
      MultiHeap.toList charHeap
    combine heap = if MultiHeap.null heap' then v
      else combine (MultiHeap.insert (k + k') (HuffNode v v') heap'')
      where
        (k,v,heap') = MultiHeap.deleteMin heap
        (k',v',heap'') = MultiHeap.deleteMin heap'

expandTreeToDictionary :: Ord a => HuffTree a -> Map.Map a [Bool]
expandTreeToDictionary (HuffLeaf c) = Map.singleton c [False]
expandTreeToDictionary tree = down Map.empty [] tree
  where
    down ret prefix (HuffLeaf c) = Map.insert c (reverse prefix) ret
    down ret prefix (HuffNode l r) =
      down (down ret (False:prefix) l) (True:prefix) r

convertToCode :: Ord a => [a] -> Map.Map a [Bool] -> [Bool]
convertToCode xs dict =
  reverse $ foldl (\ret c -> Common.revAppend ret (dict Map.! c)) [] xs

huffencode :: (Ord a, Show a) => [a] -> (HuffTree a, Bitlist.Bitlist)
huffencode [] = (HuffLeaf (error "Invalid Huffman tree"), Bitlist.empty)
huffencode xs = (tree, bitlist)
  where
    tree = buildTree xs
    dict = expandTreeToDictionary tree
    code = convertToCode xs dict
    bitlist = Bitlist.packBits code

huffdecode :: HuffTree a -> Bitlist.Bitlist -> [a]
huffdecode (HuffLeaf c) bitlist = map (\_ -> c) [1..(Bitlist.size bitlist)]
huffdecode tree bitlist =
  if Bitlist.null bitlist then []
  else decode [] tree code
  where
    code = Bitlist.unpackBits bitlist
    decode ret (HuffLeaf x) [] = reverse (x:ret)
    decode ret _ [] = error "huffdecode: did not end on leaf"
    decode ret (HuffLeaf x) bl = decode (x:ret) tree bl
    decode ret (HuffNode l _) (False:bl) = decode ret l bl
    decode ret (HuffNode _ r) (True:bl) = decode ret r bl
        
instance Binary.Binary a => Binary.Binary (HuffTree a) where
  put (HuffLeaf c) = do
    Binary.put (0 :: Word.Word8)
    Binary.put c
  put (HuffNode l r) = do
    Binary.put (1:: Word.Word8)
    Binary.put l
    Binary.put r  
  get = do
    tag <- Binary.getWord8
    case tag of
      0 -> Monad.liftM HuffLeaf Binary.get
      1 -> Monad.liftM2 HuffNode Binary.get Binary.get