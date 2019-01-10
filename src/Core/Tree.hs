module Core.Tree
  ( Tree(..)
  , root
  , tLeft
  , tRight
  ) where

-- | Trees consist of either terminal (Leaf) nodes or parent (Branch) nodes that
-- have at least one subtree. We use n-ary trees. Children are stored in
-- increasing distance from the focus, i.e. sibling indices increase relative to
-- distance.
data Tree a = Leaf a | Branch a [Tree a] (Tree a) [Tree a]

-- | Trees are functors.
instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Branch a lSibs focused rSibs) =
    let g = (fmap . fmap) f
    in  Branch (f a) (g lSibs) (f <$> focused) (g rSibs)

-- | Get the root element of a given tree.
root :: Tree a -> a
root (Leaf a) = a
root (Branch a _ _ _) = a

-- | Move "left" from the last focused child. Fails when the leftmost child was
-- last focused.
tLeft :: Tree a -> Maybe (Tree a)
tLeft (Leaf _) = Nothing
tLeft (Branch _ [] _ _) = Nothing
tLeft (Branch a (l:lSibs) focused rSibs) = Just $
  Branch a lSibs l (focused:rSibs)

-- | Move "right" from the last focused child. Fails when the rightmost child
-- was last focused.
tRight :: Tree a -> Maybe (Tree a)
tRight (Leaf _) = Nothing
tRight (Branch _ [] _ _) = Nothing
tRight (Branch a lSibs focused (r:rSibs)) = Just $
  Branch a (focused:lSibs) r rSibs
