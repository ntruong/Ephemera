module Core.Tree (Tree(..)) where

import Core.Focused (Focused(..))

-- | Trees consist of either terminal (Leaf) nodes or parent (Branch) nodes that
-- have at least one subtree. We use n-ary trees. Children are stored in
-- increasing distance from the focus, i.e. sibling indices increase relative to
-- distance.
data Tree a = Leaf a | Branch a [Tree a] (Tree a) [Tree a]

instance Focused Tree where
  f <.> (Leaf a) = Leaf (f a)
  f <.> (Branch a lSibs focused rSibs) = Branch (f a) lSibs focused rSibs
  extract (Leaf a) = a
  extract (Branch a _ _ _) = a
