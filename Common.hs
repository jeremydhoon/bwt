module Common where

revAppend :: [a] -> [a] -> [a]
revAppend acc [] = acc
revAppend acc (c:rest) = revAppend (c:acc) rest

revAppend' :: [a] -> [a] -> [a]
revAppend' acc [] = acc
revAppend' acc (c:rest) = (revAppend' $! (:) c acc) rest

