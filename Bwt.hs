module Bwt where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Control.Monad.ST as ST
import qualified Data.Array as Array
import Data.Array ((!))
import qualified Data.Array.Unboxed as UArray
import qualified Data.Array.Base as BaseArray
import qualified Data.Array.ST as STArray
import qualified Data.Binary as Binary
import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import Data.Int (Int64)
import qualified Data.Ix as Ix
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Debug.Trace as Trace
import qualified Data.Word as Word

import qualified Bitlist
import qualified Common
import qualified HuffTree as Huff
import qualified UnicodeMtf as Mtf

type DefaultArray = Array.Array

type Fixstr = DefaultArray Int Char
data Rotstr = Rotstr {
  rotstrFs :: Fixstr,
  rotstrOff :: Int,
  rotstrSize :: Int
}

instance Eq Rotstr where
  a == b = (stringFromRotstr a) == (stringFromRotstr b)

instance Ord Rotstr where
  compare a@(Rotstr fsA offA szA) b@(Rotstr fsB offB szB) = down 0
    where
      end = min szA szB
      down i = if i >= end then compare szA szB else case outcome of
        EQ -> down (i + 1)
        _ -> outcome
        where
          outcome = compare (rotstrIndex a i) (rotstrIndex b i)

type IntArray = UArray.UArray Int Int

binSizesFromIndices :: (Int -> Int) -> [Int] -> IntArray
binSizesFromIndices keyfn vs = STArray.runSTUArray $ do
  let ks = map keyfn vs
  let mn = List.foldl' min (head ks) (tail ks)
  let mx = List.foldl' max (head ks) (tail ks)
  let limits = (mn,mx)
  arr <- STArray.newArray limits 0
  Monad.forM_ ks (\k -> do
    count <- STArray.readArray arr k
    STArray.writeArray arr k (count + 1))
  return arr

cumulativeBinSizes :: IntArray -> IntArray
cumulativeBinSizes binSizes = UArray.array (mn, mx + 1) $ zip [mn..] cumSizes
  where
    update ns@(hd:_) n = seq hd' $ hd':ns
      where hd' = hd + n
    elems = UArray.elems binSizes
    (mn,mx) = UArray.bounds binSizes
    cumSizes = reverse $! List.foldl' update [0] elems

distributeAndSortIndices :: (Int -> Int -> Ordering) -> (Int -> Int) ->
  IntArray -> (Int, Int) -> IntArray
distributeAndSortIndices cmp keyfn cumSizes bounds = STArray.runSTUArray $ do
  let (mn,mx) = bounds
  arr <- (STArray.newArray_ bounds) :: (ST.ST s) (STArray.STUArray s Int Int)
  offs <- (STArray.thaw cumSizes) :: (ST.ST s) (STArray.STUArray s Int Int)
  mapM_ (\i -> do
    let k = keyfn i
    ix <- STArray.readArray offs k
    STArray.writeArray offs k (ix + 1)
    STArray.writeArray arr ix i) [mn..mx]
  Foldable.foldlM (\prev limit -> do
    qsortSegment cmp arr prev (limit - 1)
    return limit)
    0 (List.tail $ UArray.elems cumSizes)
  return arr
    
sortElementsInPlace cmp keyfn bounds = 
  UArray.elems $ distributeAndSortIndices cmp keyfn cumulativeSizes bounds
  where
    (mn,mx) = bounds
    binSizes = binSizesFromIndices keyfn [mn..mx]
    cumulativeSizes = cumulativeBinSizes binSizes
  
readArray :: STArray.STUArray s Int Int -> Int -> (ST.ST s) Int
--readArray = BaseArray.unsafeRead
readArray = STArray.readArray

writeArray :: STArray.STUArray s Int Int -> Int -> Int -> (ST.ST s) ()
--writeArray = BaseArray.unsafeWrite
writeArray = STArray.writeArray

swapIndices arr i j = do
  ival <- readArray arr i
  jval <- readArray arr j
  writeArray arr i jval
  writeArray arr j ival

loop :: (Monad m) => (a -> Int -> m a) -> a -> Int -> Int -> m a
loop f acc start end =
  if start > end
    then
      return acc
    else do
      let start' = start + 1
      acc' <- f acc start
      loop f acc' (seq2 start' acc' start') end

partition cmp arr start end = do
  let swap = swapIndices arr
  pivot <- STArray.readArray arr start
  swap start end
  pivot' <- loop (\slot i -> do
    ival <- readArray arr i
    case cmp ival pivot of
      GT -> return slot
      _ -> do
        swap i slot
        return (slot + 1))
    start start (end - 1)
  swap pivot' end
  return pivot'

qsortSegment cmp arr start end = Monad.when (start < end) $ do
  pivot <- partition cmp arr start end
  qsortSegment cmp arr start (pivot - 1)
  qsortSegment cmp arr (pivot + 1) end

qsort :: (Int -> Int -> Ordering) -> (STArray.STUArray s Int Int) ->
  (ST.ST s) ()
qsort cmp arr = do 
  (start,end) <- STArray.getBounds arr
  qsortSegment cmp arr start end

qsortList cmp xs = UArray.elems $ STArray.runSTUArray $ do
  let n = length xs
  arr <- STArray.newListArray (0, n-1) xs
  qsort cmp arr
  return arr

sortListInPlace :: (Int -> Int -> Ordering) -> [Int] -> [Int]
sortListInPlace cmp xs = UArray.elems $ STArray.runSTUArray $ do
  let len = length xs
  arr <- (STArray.newListArray (0, len-1) xs) ::
    (ST.ST s) (STArray.STUArray s Int Int)
  qsort cmp arr
  return arr

buildSecondKeyLookup :: (Int -> Int64) -> Int -> (Int, Int) -> (Int -> Int64)
buildSecondKeyLookup keyfn keyOffset bounds = (arr UArray.!)
  where
    inds = map (keyfn . ((+) keyOffset)) [(fst bounds)..(snd bounds)]
    arr = (UArray.listArray bounds inds) :: UArray.UArray Int Int64

bwtFast :: [Word.Word8] -> (Int, [Word.Word8])
bwtFast [] = (0, [])
bwtFast s = (ixEnd, last)
  where
    n = length s
    fs = UArray.listArray (0, n - 1) s
    indices = Array.indices fs
    get = (fs !)
    getMod i = fs ! (i `mod` n)
    maxEle =
      Exception.assert ((fromIntegral $ (minBound :: Word.Word8)) == 0) $
      fromIntegral (maxBound :: Word.Word8)
    shift = maxEle + 1
    getRank i= (fromIntegral (get i))
    getModRank i = (fromIntegral (getMod i))
    buildKeyfn count = \v -> List.foldl' (update v) (0 :: Int64) inds
      where
        update v acc i = (acc * shift) + (getModRank (v + i))
        inds = [0..(count-1)]
    keyOffset = min n 2
    keyfn = buildKeyfn keyOffset
    lastMod i = fs ! ((i + (n - 1)) `mod` n)
    cmp i j = case rem of
      [] -> EQ
      (hd:_) -> checkIx hd
      where
        checkIx k = compare (getMod (k + i)) (getMod (k + j))
        rem = List.dropWhile (\ix -> checkIx ix == EQ) indices
    int64Width = 8
    secondKey = buildSecondKeyLookup (buildKeyfn int64Width) keyOffset (0, n-1)
    secondKeyOffset = keyOffset + int64Width
    cmp' i j = case compare (secondKey i) (secondKey j) of
      LT -> LT
      GT -> GT
      EQ -> cmp (i + secondKeyOffset) (j + secondKeyOffset)
    inPlaceLimit = fromIntegral $ (maxEle ^ keyOffset) `div` 4
    sorted =
      if n < inPlaceLimit then
        List.sortBy cmp indices
      else
        sortElementsInPlace cmp' (fromIntegral . keyfn) (0, (n-1))
    --sorted = sortBins cmp bins
    --sorted = List.sortBy cmp indices
    last = map lastMod sorted
    ixEnd = case  List.elemIndex 0 sorted of
      Just i -> i
      Nothing -> error "zero index not found"

buildFixstr :: String -> Fixstr
buildFixstr s = Array.listArray (0, (length s) - 1) s

rotstrIndex :: Rotstr -> Int -> Char
rotstrIndex (Rotstr fs off n) i = fs ! ((off + i) `mod` n)

rotstrLastChar :: Rotstr -> Char
rotstrLastChar rs@(Rotstr fs off n) = rotstrIndex rs (n - 1)

stringFromRotstr :: Rotstr -> String
stringFromRotstr rs@(Rotstr {rotstrSize = n}) =
  reverse $ List.foldl' (\acc i -> (rotstrIndex rs i):acc) [] [0..(n-1)]

buildRotstr :: Fixstr -> Int -> Rotstr
buildRotstr fs off = Rotstr fs off (1 + (snd $ Array.bounds fs))

bwt :: String -> (Int, String)
bwt [] = (0, [])
bwt s = (ix, mcs)
  where
    fs = buildFixstr s
    --sortFn = rotstrSort
    sortFn = List.sort
    tbl = sortFn $ map (buildRotstr fs) [0..((length s) - 1)]
    mcs = map rotstrLastChar tbl   
    ix = case List.findIndex (\(Rotstr {rotstrOff = off}) -> off == 0) tbl of
      Just i -> i
      Nothing -> error "bwt: no zero-index rotation"

addAndSort :: [[Maybe Char]] -> [Maybe Char] -> [[Maybe Char]]
addAndSort ss base = List.sort ss'
  where
    ss' = map (\(c,s) -> c:s) (zip base ss)

type Shiftvector = DefaultArray Int Int

buildBwtShiftVector :: String -> Shiftvector
buildBwtShiftVector s = Array.listArray (0, (length tlist) - 1) tlist
  where
    f = List.sort s
    handleChr (mSearchStr, ret) c =
      seq (seq mSearchStr' nextRet) (mSearchStr', nextRet)
      where
        (s', off) = Map.findWithDefault (f, 0) c mSearchStr
        (prefix,suffix) = List.break (\c' -> c == c') s'
        ix = off + (length prefix)
        mSearchStr' = Map.insert c (tail suffix, ix + 1) mSearchStr
        nextRet = ix:ret
    tlist = reverse $! snd $! List.foldl' handleChr (Map.empty, []) s

seq2 a b c = seq (seq a b) c
seq3 a b c d = seq (seq2 a b c) d
seq4 a b c d e = seq (seq3 a b c d) e

buildBwtShiftVectorFast :: [Word.Word8] -> Shiftvector
buildBwtShiftVectorFast s = Array.listArray (0, n - 1) tlist
  where
    n = length s
    --f = qsortList compare s
    f = List.sort s
    addIndex m (c,i) = seq3 is is' m' m'
      where
        is = Map.findWithDefault [] c m
        is' = i:is
        m' = Map.insert c is' m
    charIndices = List.foldl' addIndex Map.empty $ zip f [0..]
    removeIndex (m,tlist) c = seq3 hd m' tlist' (m',tlist')
      where
        (hd:is) = m Map.! c
        m' = Map.insert c is m
        tlist' = hd:tlist
    tlist = snd $ List.foldl' removeIndex (charIndices,[]) $! reverse s

reverseBwtFast :: [Word.Word8] -> Int -> [Word.Word8]
reverseBwtFast [] _ = []
reverseBwtFast s ixEnd =
  Exception.assert (ixEnd <= (length s) && ixEnd >= 0) s'
  where
    len = length s
    t = buildBwtShiftVectorFast s
    l = (Array.listArray (0, (length s) - 1) s) :: DefaultArray Int Word.Word8
    getc = (l !)
    gett = (t !)
    nextChar (cs,i) _ = seq2 cs' i' (cs', i')
      where
        c = getc i
        cs' = seq c (c:cs)
        i' = gett i
    s' = fst $ List.foldl' nextChar ([], ixEnd) [1..len]
   
data RLList a = RLList [(a, Int)]

instance Show a => Show (RLList a) where
  show (RLList pairs) = "RLList " ++ (show pairs)

runLengthEncode :: Eq a => [a] -> RLList a
runLengthEncode [] = RLList []
runLengthEncode s = RLList $ reverse $ (prev,n):pairs
  where
    countC (prev,n,acc) c = if c == prev then (c,n+1,acc)
      else (c,1,(prev,n):acc)
    (prev,n,pairs) = List.foldl' countC (head s, 1, []) $ tail s

runLengthDecodePairs :: (Eq a, Integral b) => [(a, b)] -> [a]
runLengthDecodePairs pairs = List.foldl' Common.revAppend [] dupls
  where
    duplicate c n = List.foldl' (\cs _ -> c:cs) [] [0..(n-1)]
    dupls = List.foldl' (\acc pair -> ((uncurry duplicate) pair):acc) [] pairs

runLengthDecode :: Eq a => RLList a -> [a]
runLengthDecode (RLList pairs) = runLengthDecodePairs pairs

expandRlList :: RLList a -> [(a, Word.Word8)]
expandRlList (RLList pairs) = handlePair [] pairs
  where 
    wordlimit = 255
    wordlimit8 = (fromIntegral wordlimit) :: Word.Word8
    handlePair ret [] = reverse ret
    handlePair ret ((c,i):rest) =
      if i <= wordlimit then handlePair ((c, fromIntegral i):ret) rest
      else handlePair ((c, wordlimit8):ret) $ (c, i - wordlimit):rest

buildIsRlList :: Integral a => [(b,a)]  -> [Bool]
buildIsRlList = map (\(_,i) -> i > 1)

splitRlPairs :: Integral a => [(b,a)] -> ([a],[b])
splitRlPairs pairs = (reverse accCount, reverse accChar)
  where
    handlePair (accCount, accChar) (c,i) =
      if i > 1 then (i:accCount, c:accChar) else (accCount, c:accChar)
    (accCount,accChar) = List.foldl' handlePair ([],[]) pairs

assembleRLList :: [Bool] -> [a] -> [Word.Word8] -> RLList a
assembleRLList isRl chars counts = RLList $! consume [] isRl chars counts
  where
    consume ret [] [] [] = reverse ret
    consume ret (True:rlRest) (c:cRest) (n:nRest) =
      seq ret' $ consume ret' rlRest cRest nRest
      where
        n' = fromIntegral n
        rethd = seq n' (c,n')
        ret' = seq rethd $ rethd:ret
    consume ret (False:rlRest) (c:cRest) ns =
      seq ret' $ consume ret' rlRest cRest ns
      where
        rethd = (c,1)
        ret' = seq rethd $ rethd:ret 
    consume _ [] _ _ = error "assembleRLList: ran out of flags"
    consume _ _ [] _ = error "assembleRLList: ran out of chars"
    consume _ _ _ [] = error "assembleRLList: ran out of counts"

data CodedBlock = CodedBlock {
     codedblockIxEnd :: Int,
     codedblockLengthHuffTree :: Huff.HuffTree Word.Word8,
     codedblockSymbolHuffTree :: Huff.HuffTree Int,
     codedblockIsRlBits :: Bitlist.Bitlist,
     codedblockLengthBits :: Bitlist.Bitlist,
     codedblockSymbolsBits :: Bitlist.Bitlist
}

encode :: [Word.Word8] -> CodedBlock
encode [] = error "Cannot encode an empty string."
encode xs =
  CodedBlock ixEnd treeLengths treeSymbols bitsIsRl bitsLengths bitsSymbols
  where
    log a b = b --Trace.trace a b
    (ixEnd,bwtEnc) = log "bwt" $! bwtFast xs
    mtfEnc = log "mtf" $! Mtf.mtf bwtEnc
    rl = log "rle" $! expandRlList $! runLengthEncode mtfEnc
    (lengths,symbols) = splitRlPairs rl
    (treeLengths,bitsLengths) = log "huff lengths" $! Huff.huffencode lengths
    (treeSymbols,bitsSymbols) = log "huff symbols" $! Huff.huffencode symbols
    bitsIsRl = Bitlist.packBits $! buildIsRlList rl

decode :: CodedBlock -> [Word.Word8]
decode (CodedBlock ixEnd treeLen treeSymb bitsIsRl bitsLen bitsSym) = s
  where
    log a b = b -- Trace.trace a b
    lengths = log "dec lengths" $! Huff.huffdecode treeLen bitsLen
    symbols = log "dec symbols" $! Huff.huffdecode treeSymb bitsSym
    flags = log "dec flags" $! Bitlist.unpackBits bitsIsRl
    rl = log "assembling rl list" $! assembleRLList flags symbols lengths
    mtfDec = log "dec rle, mtf" $! Mtf.reverseMtf $! runLengthDecode rl
    s = log "dec bwt" $! reverseBwtFast mtfDec ixEnd

instance Binary.Binary CodedBlock where
  put (CodedBlock ix treeLen treeSymb bitsIsRl bitsLen bitsSymb) = do
    Binary.put ix
    Binary.put treeLen
    Binary.put treeSymb
    Binary.put bitsIsRl
    Binary.put bitsLen
    Binary.put bitsSymb
  get = do
   ix <- Binary.get
   treeLen <- Binary.get
   treeSymb <- Binary.get
   bitsIsRl <- Binary.get
   bitsLen <- Binary.get
   bitsSymb <- Binary.get
   return $ CodedBlock ix treeLen treeSymb bitsIsRl bitsLen bitsSymb