module UnicodeMtf where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Word as Word
import qualified Debug.Trace as Trace

data ByteMap = ByteMap {
  unicodemapByteToIx :: Map.Map Word.Word8 Int,
  unicodemapIxToByte :: Map.Map Int Word.Word8,
  unicodemapNext :: Int
}

empty = ByteMap Map.empty Map.empty (-1)

put :: Word.Word8 -> ByteMap -> ByteMap
put c (ByteMap c2i i2c next) =
  ByteMap (Map.insert c next c2i) (Map.insert next c i2c') (next - 1)
  where
    i2c' = if Map.member c c2i then Map.delete (c2i Map.! c) i2c else i2c

getRank :: Word.Word8 -> ByteMap -> Maybe Int
getRank c um@(ByteMap c2i i2c next) = do
  key <- Map.lookup c c2i
  Map.lookupIndex key i2c

getCharForRank :: Int -> ByteMap -> Maybe Word.Word8
getCharForRank ix um@(ByteMap c2i i2c next) = 
  if ix < Map.size i2c then Just $ snd $ Map.elemAt ix i2c else Nothing

getIx :: Word.Word8 -> ByteMap -> (ByteMap, Int)
getIx c um@(ByteMap c2i i2c next) = case getRank c um of
  Just i -> (um',i)
  Nothing -> (um', ix)
  where
    cval = fromIntegral c
    c2i' = Map.insert c cval c2i
    ahead = Map.findIndex c c2i'
    ix =
      cval +  (Map.size c2i) - ahead
    um' = put c um

getMissingC :: Int -> Map.Map Word.Word8 Int -> Word.Word8
getMissingC ix c2i = fromIntegral $ findPlace ix keySet
  where
    keySet = Map.keysSet c2i
    findPlace i s = if (not $ Set.null s) && i <= maxval
      then findPlace (i - 1) s' else i
      where
        (mx,s') = Set.deleteFindMax s
        maxval = fromIntegral mx
 
getC :: Int -> ByteMap -> (ByteMap, Word.Word8)
getC ix um@(ByteMap c2i i2c next) = case getCharForRank ix um of
  Just x -> (put x um, x)
  Nothing -> (put c um, c)
  where
    c = getMissingC ix c2i

reverseSnd :: (a,[b]) -> [b]
reverseSnd = reverse . snd

mtf :: [Word.Word8] -> [Int]
mtf s = reverseSnd $ foldl addIx (empty,[]) s
  where
    addIx (um,ret) c = (um', ix:ret)
      where
        (um',ix) = getIx c um
    
reverseMtf :: [Int] -> [Word.Word8]
reverseMtf is = reverseSnd $ List.foldl' addC (empty, []) is
  where
    addC (um,ret) i = seq (seq um' ret') (um', ret')
      where
        (um',c) = getC i um
        ret' = seq c $ c:ret
