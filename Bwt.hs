module Bwt where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Control.Monad.ST as ST
import qualified Data.Array as Array
import qualified Data.Array.Unboxed as UArray
import qualified Data.Array.ST as STArray
import qualified Data.Binary as Binary
import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
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

(!) = (Array.!)
{-
(!) :: (Show i, Show e, Ix.Ix i) => DefaultArray i e -> i -> e
arr ! i =
  if i >= mn && i <= mx then
    arr Array.! i
  else
    error ("Invalid array access: " ++ (show i) ++ " on " ++ (show arr))
  where
    (mn,mx) = Array.bounds arr
-}

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

findLimits :: (Show key, Ix.Ix key) => (val -> key) -> [val] -> (key, key)
findLimits _ [] = error "findLimits: cannot find limits of empty list."
findLimits keyfn (v:vs) = Trace.trace (show $ k1:keys) $
  List.foldl' (\(mn,mx) k -> (min mn k, max mx k)) (k1, k1) keys
  where
    keys = map keyfn vs
    k1 = keyfn v

getCircular :: DefaultArray Int Char -> Int -> Char
getCircular a i = (!) a $! ((i - mn) `mod` (mx - mn + 1)) + mn
  where
    (mn,mx) = Array.bounds a

buildKeyfn :: DefaultArray Int Char -> Int -> (Int -> Int)
buildKeyfn fs n = Trace.trace (show fs) $
  (keysArray !)
  where
    indices = Array.indices fs
    n = (snd $ Array.bounds fs) + 1
    get = (fs !)
    cmp i j = compare (get i) (get j)
    extractOrd = Char.ord . get
    maxEle = extractOrd $! List.maximumBy cmp indices
    minEle = extractOrd $! List.minimumBy cmp indices
    shift = Trace.trace (show maxEle) $ (maxEle - minEle) + 1
    getOMod = Char.ord . (getCircular fs)
    consume i tot j = Exception.assert (cOrd >= 0) $ (tot * shift) + cOrd
      where
        cOrd = (getOMod (i + j)) - minEle
    keys = map (\i -> foldl (consume i) 0 [0..(n-1)]) indices
    keysArray = Array.listArray (Array.bounds fs) keys

buildCountArray :: DefaultArray Int Char -> (Int, Int) -> (Int -> Int) ->
  DefaultArray Int Int
buildCountArray a limits keyfn = STArray.runSTArray $ do
  let indices = Array.indices a
  arr <- STArray.newArray limits 0
  assocs <- STArray.getAssocs arr
  let arrIndices = map fst assocs
  mapM (\i -> do
    let k = keyfn i
    count <- STArray.readArray arr k
    STArray.writeArray arr k (count + 1))
    indices
  mapM (\(prev,curr) -> do
   prevCount <- STArray.readArray arr prev
   currCount <- STArray.readArray arr curr
   STArray.writeArray arr curr (prevCount + currCount)) $
   zip arrIndices (tail arrIndices)
  return arr
  
swapElements arr i j = do
  ival <- STArray.readArray arr i
  jval <- STArray.readArray arr j
  STArray.writeArray arr i jval
  STArray.writeArray arr j ival

qsortSegment arr keyfn start len = Monad.when (len > 0) $ do
  pivotValue <- STArray.readArray arr start
  let pivot = keyfn pivotValue
  let swap = swapElements arr
  let right = start + len - 1
  swap start right
  slot <- Foldable.foldlM (\slot i -> do
    currValue <- STArray.readArray arr i
    let currKey = keyfn currValue
    if currKey <= pivot
      then do
        swap slot i
        return (slot + 1)
      else do
        return slot)
    start [start..(right - 1)]
  swap slot right
  qsortSegment arr keyfn start (slot - start - 1)
  qsortSegment arr keyfn (slot + 1) (right - slot)

sortBinsInpl :: DefaultArray Int Int -> DefaultArray Int Char -> (Int, Int)
  -> (Int -> Int) -> [Int]
sortBinsInpl counts fs limits keyfn = Array.elems $ STArray.runSTArray $ do
  let cindices = Array.indices counts
  let bounds = (Array.bounds fs) :: (Int, Int)
  let _ = Trace.trace "buiding off" ()
  off <- (STArray.newArray limits 0) :: ((ST.ST s) (STArray.STArray s Int Int))
  monolithicArray <- STArray.newArray_ bounds
  Monad.forM_ (Array.indices fs ) (\i -> do
    let k = keyfn i
    let binOff = counts ! k
    binIx <- STArray.readArray off k
    let ix = (binOff + binIx) :: Int
    STArray.writeArray off (k :: Int) ((binIx + 1) :: Int)
    STArray.writeArray monolithicArray ix i)
  let (mn,mx) = bounds
  let _ = Trace.trace "qsorting..." ()
  qsortSegment monolithicArray keyfn mn (mx - mn + 1)
  return monolithicArray

bwtInPlace :: String -> (Int, String)
bwtInPlace [] = (0, [])
bwtInPlace (hd:[]) = (1, [hd])
bwtInPlace s = (ixEnd, map (\i -> getCircular fs (i + n - 1)) sortedIndices)
  where
    n = length s
    fs = buildFixstr s
    keyfn = buildKeyfn fs 2
    limits = findLimits keyfn (Array.indices fs)
    counts = Trace.trace (show limits) $ buildCountArray fs limits keyfn
    sortedIndices = Trace.trace (show counts) $ sortBinsInpl counts fs limits keyfn
    ixEnd = case List.elemIndex 0 sortedIndices of
      Just i -> i
      Nothing -> error "bwtInPlace: no zero index" 

binElements :: Ix.Ix key => (val -> key) -> [val] -> Array.Array key [val]
binElements _ [] = error "binElements: cannot bin empty list"
binElements keyfn vs = STArray.runSTArray $ do
  let hd = keyfn $ head vs
  let extrema (mn,mx) k = (min mn $! keyfn k, max mx $! keyfn k)
  let limits = List.foldl' extrema (hd,hd) vs
  binArray <- STArray.newArray limits []
  mapM (\v -> do
    let k = keyfn v
    bin <- (STArray.readArray binArray k)
    STArray.writeArray binArray k (v:bin))
    vs
  return binArray

sortBins :: (val -> val -> Ordering) -> Array.Array Int [val] -> [val]
sortBins cmp bins = reverse $! List.foldl' Common.revAppend' [] $! sbins
  where
    sortFn = List.sortBy cmp
    getBinAndSort i = sortFn $! bins ! i
    sbins = map getBinAndSort $! Array.indices bins

bwtFast :: String -> (Int, String)
bwtFast [] = (0, [])
bwtFast s = (ixEnd, last)
  where
    fs = buildFixstr s
    n = (snd $ Array.bounds fs) + 1
    indices = Array.indices fs
    get = (fs !)
    getMod i = fs ! (i `mod` n)
    maxEle = Char.ord $! List.maximum s
    keyfn = if n > 1 then
      \v -> ((Char.ord $ get v) * maxEle) + (Char.ord $ getMod (v+1))
      else Char.ord . get
    lastMod i = fs ! ((i + (n - 1)) `mod` n)
    bins = binElements keyfn indices
    cmp i j = case rem of
      [] -> EQ
      (hd:_) -> checkIx hd
      where
        checkIx k = compare (getMod (k + i)) (getMod (k + j))
        rem = List.dropWhile (\ix -> checkIx ix == EQ) indices
    sorted = sortBins cmp bins
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
buildBwtShiftVector s = UArray.listArray (0, (length tlist) - 1) tlist
  where
    f = List.sort s
    handleChr (mSearchStr, ret) c =
      Exception.assert (ix < (length f) && ix >= 0) (mSearchStr', ix:ret)
      where
        (s', off) = Map.findWithDefault (f, 0) c mSearchStr
        (prefix,suffix) = List.break (\c' -> c == c') s'
        ix = off + (length prefix)
        mSearchStr' = Map.insert c (tail suffix, ix + 1) mSearchStr
    tlist = reverse $ snd $ List.foldl' handleChr (Map.empty, []) s

reverseBwtFast :: String -> Int -> String
reverseBwtFast [] _ = []
reverseBwtFast s ixEnd =
  Exception.assert (ixEnd <= (length s) && ixEnd >= 0) s'
  where
    len = length s
    t = buildBwtShiftVector s
    l = (UArray.listArray (0, (length s) - 1) s) :: DefaultArray Int Char
    getc = (l !)
    gett = (t !)
    getNext ret i sz = --Exception.assert (i < (length s) && i >= 0) $
      if sz' == len then ret' else getNext ret' ti sz'
      where
        ret' = (getc i):ret
        ti = gett i
        sz' = sz + 1
    s' = getNext [] ixEnd 0
   
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
runLengthDecodePairs pairs = down [] pairs
  where
    down ret [] = reverse ret
    down ret ((c,0):rest) = down ret rest
    down ret ((c,i):rest) = down (c:ret) ((c,i-1):rest)

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
assembleRLList isRl chars counts = RLList $ consume [] isRl chars counts
  where
    consume ret [] [] [] = reverse ret
    consume ret (True:rlRest) (c:cRest) (n:nRest) =
      consume ((c,fromIntegral n):ret) rlRest cRest nRest
    consume ret (False:rlRest) (c:cRest) ns =
      consume ((c,1):ret) rlRest cRest ns
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

encode :: String -> CodedBlock
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

decode :: CodedBlock -> String
decode (CodedBlock ixEnd treeLen treeSymb bitsIsRl bitsLen bitsSym) = s
  where
    log a b = b --Trace.trace
    lengths = log "dec lengths" $ Huff.huffdecode treeLen bitsLen
    symbols = log "dec symbols" $ Huff.huffdecode treeSymb bitsSym
    flags = log "dec flags" $ Bitlist.unpackBits bitsIsRl
    rl = log "assembling rl list" $ assembleRLList flags symbols lengths
    mtfDec = log "dec rle, mtf" $ Mtf.reverseMtf $ runLengthDecode rl
    s = log "dec bwt" $ reverseBwtFast mtfDec ixEnd

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