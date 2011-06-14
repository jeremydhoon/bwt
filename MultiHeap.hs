module MultiHeap where

import qualified Data.Map as Map
import qualified Debug.Trace as Trace

data MultiHeap key val = MultiHeap (Map.Map (key, Int) val, Int)

empty :: MultiHeap key val
empty = MultiHeap (Map.empty, 0)

null :: MultiHeap key val -> Bool
null (MultiHeap (m,_)) = Map.null m

insert :: Ord key => key -> val -> MultiHeap key val -> MultiHeap key val
insert k v (MultiHeap (m,i)) = MultiHeap (Map.insert (k,i) v m, i + 1)

deleteMin :: Ord key => MultiHeap key val -> (key, val, MultiHeap key val)
deleteMin (MultiHeap (m,i)) = (k, v, MultiHeap (m',i))
  where
    (((k,_),v),m') = Map.deleteFindMin m

toList :: Ord key => MultiHeap key val -> [(key, val)]
toList (MultiHeap (m,_)) = map (\((a,_),b) -> (a,b)) $ Map.toList m

fromList :: Ord key => [(key, val)] -> MultiHeap key val
fromList xs =
  MultiHeap (Map.fromList (map (\((k,v),i) -> ((k,i),v)) $
  zip xs [0..((length xs) - 1)]), length xs)