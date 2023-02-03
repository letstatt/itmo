module HW1.T6 (mcat, epart) where

mcat :: Monoid t => [Maybe t] -> t
mcat =
  foldl
    ( \b a -> case a of
        Nothing -> b
        Just c  -> b <> c
    )
    mempty

-- omg, string is a monoid *_*

-- it's all the same
epart :: (Monoid x, Monoid y) => [Either x y] -> (x, y)
epart =
  foldl
    ( \(a, b) c -> case c of
        Left l  -> (a <> l, b)
        Right r -> (a, b <> r)
    )
    (mempty, mempty)

-- ok
