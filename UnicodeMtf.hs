module UnicodeMtf where

import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Debug.Trace as Trace

data UnicodeMap = UnicodeMap {
  unicodemapCharToIx :: Map.Map Char Int,
  unicodemapIxToChar :: Map.Map Int Char,
  unicodemapNext :: Int
}

empty = UnicodeMap Map.empty Map.empty (-1)

put :: Char -> UnicodeMap -> UnicodeMap
put c (UnicodeMap c2i i2c next) =
  UnicodeMap (Map.insert c next c2i) (Map.insert next c i2c') (next - 1)
  where
    i2c' = if Map.member c c2i then Map.delete (c2i Map.! c) i2c else i2c

getRank :: Char -> UnicodeMap -> Maybe Int
getRank c um@(UnicodeMap c2i i2c next) = do
  key <- Map.lookup c c2i
  Map.lookupIndex key i2c

getCharForRank :: Int -> UnicodeMap -> Maybe Char
getCharForRank ix um@(UnicodeMap c2i i2c next) = 
  if ix < Map.size i2c then Just $ snd $ Map.elemAt ix i2c else Nothing

getIx :: Char -> UnicodeMap -> (UnicodeMap, Int)
getIx c um@(UnicodeMap c2i i2c next) = case getRank c um of
  Just i -> (um',i)
  Nothing -> (um', ix)
  where
    cval = Char.ord c
    c2i' = Map.insert c cval c2i
    ahead = Map.findIndex c c2i'
    ix = --Trace.trace ((show c) ++ ", " ++ (show ahead)) $
      cval +  (Map.size c2i) - ahead
    um' = put c um

getMissingC :: Int -> Map.Map Char Int -> Char
getMissingC ix c2i = Char.chr $ findPlace ix keySet
  where
    keySet = Map.keysSet c2i
    findPlace i s = if (not $ Set.null s) && i <= maxval
      then findPlace (i - 1) s' else i
      where
        (max,s') = Set.deleteFindMax s
        maxval = Char.ord max
 
getC :: Int -> UnicodeMap -> (UnicodeMap, Char)
getC ix um@(UnicodeMap c2i i2c next) = case getCharForRank ix um of
  Just x -> (put x um, x)
  Nothing -> (put c um, c)
  where
    c = getMissingC ix c2i

reverseSnd :: (a,[b]) -> [b]
reverseSnd = reverse . snd

mtf :: String -> [Int]
mtf s = reverseSnd $ foldl addIx (empty,[]) s
  where
    addIx (um,ret) c = (um', ix:ret)
      where
        (um',ix) = getIx c um
    
reverseMtf :: [Int] -> String
reverseMtf is = reverseSnd $ foldl addC (empty, []) is
  where
    addC (um,ret) i = (um', c:ret)
      where
        (um',c) = getC i um
