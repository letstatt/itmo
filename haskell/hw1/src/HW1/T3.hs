module HW1.T3
  ( Meta (Empty, M),
    Tree (Leaf, Branch),
    tsize,
    tdepth,
    tmember,
    tinsert,
    tFromList,
  )
where

data Meta = Empty | M Int Int -- size and height in AVL subtree

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)

-- | Size of the tree, O(1).
tsize :: Tree a -> Int
tsize Leaf                      = 0
tsize (Branch (M size _) _ _ _) = size
tsize _                         = undefined

-- | Depth of the tree.
tdepth :: Tree a -> Int
tdepth Leaf                        = 0
tdepth (Branch (M _ height) _ _ _) = height
tdepth _                           = undefined

-- | Balance factor
bfactor :: Tree a -> Int
bfactor Leaf             = 0
bfactor (Branch _ x _ z) = tdepth z - tdepth x

-- | Make branch
mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch x y z = updateMeta $ Branch Empty x y z

-- | Update info in a branch
updateMeta :: Tree a -> Tree a
updateMeta Leaf = Leaf
updateMeta (Branch _ x y z) = Branch (M (tsize x + tsize z + 1) (1 + max (tdepth x) (tdepth z))) x y z

-- | balancing
rotateRight :: Tree a -> Tree a
rotateRight (Branch _ (Branch _ x1 y1 z1) y z) = mkBranch x1 y1 (mkBranch z1 y z)
rotateRight _                                  = undefined

rotateLeft :: Tree a -> Tree a
rotateLeft (Branch _ x y (Branch _ x1 y1 z1)) = mkBranch (mkBranch x y x1) y1 z1
rotateLeft _                                  = undefined

balanceRight :: Tree a -> Tree a
balanceRight b@(Branch _ x y z) =
  if bfactor b == 2
    then
      rotateLeft
        ( if bfactor z < 0 then mkBranch x y $ rotateRight z else b
        )
    else b
balanceRight _ = undefined

balanceLeft :: Tree a -> Tree a
balanceLeft b@(Branch _ x y z) =
  if bfactor b == -2
    then
      rotateRight
        ( if bfactor x > 0 then mkBranch (rotateLeft x) y z else b
        )
    else b
balanceLeft _ = undefined

balance :: Tree a -> Tree a
balance = balanceLeft . balanceRight

-- | Check if the element is in the tree, O(log n)
tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember a (Branch _ x y z)
  | a == y = True
  | a < y = tmember a x
  | otherwise = tmember a z

-- | Insert an element into the tree, O(log n)
tinsert :: Ord a => a -> Tree a -> Tree a
tinsert a Leaf = mkBranch Leaf a Leaf
tinsert a b@(Branch _ x y z)
  | a < y = balance $ mkBranch (tinsert a x) y z
  | a == y = b
  | otherwise = balance $ mkBranch x y (tinsert a z) -- othewise means a > y, I hope

-- | Build a tree from a list, O(n log n)
tFromList :: Ord a => [a] -> Tree a
tFromList = insert
  where
    insert arg = case arg of
      (x : xs) -> tinsert x $ insert xs
      []       -> Leaf
