module HW1.T4 (tfoldr, treeToList) where

import HW1.T3 (Tree (..))

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr f n tree = tfoldrimpl tree n
  where
    tfoldrimpl t acc = case t of
      Leaf -> acc
      (Branch _ x y z) -> tfoldrimpl x intermediate
        where
          intermediate = f y $ tfoldrimpl z acc

treeToList :: Tree a -> [a] -- output list is sorted
treeToList = tfoldr (:) []
