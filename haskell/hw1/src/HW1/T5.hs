module HW1.T5 (splitOn, joinWith) where

import Data.List.NonEmpty (NonEmpty (..), (<|))

splitOnImpl :: Eq a => a -> a -> NonEmpty [a] -> NonEmpty [a]
splitOnImpl sep c list@(x :| xs)
  | c == sep = [] <| list -- add one more list
  | otherwise = (c : x) :| xs

--splitOn sep = foldr (splitOnImpl sep) $ [] :| []
--lets point free
splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn = ($ [] :| []) . foldr . splitOnImpl

joinWith :: a -> NonEmpty [a] -> [a]
joinWith _ (x :| [])   = x
joinWith sep (x :| xs) = x ++ foldl (\i j -> i ++ [sep] ++ j) [] xs

-- wow, i'm genius
