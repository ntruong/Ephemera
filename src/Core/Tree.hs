module Core.Tree
  ( Tree(..)
  , root
  , tLeft
  , tRight
  , tInsLeft
  , tInsRight
  , tDelete
  , tLeftmost
  , tRightmost
  , tSortOn
  , tReverse
  ) where

import qualified Data.List as L (sortOn)

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
tRight (Branch _ _ _ []) = Nothing
tRight (Branch a lSibs focused (r:rSibs)) = Just $
  Branch a (focused:lSibs) r rSibs

-- | Insert a subtree "left" of the last focused child and focus it.
tInsLeft :: Tree a -> Tree a -> Tree a
tInsLeft t (Leaf a) = Branch a [] t []
tInsLeft t (Branch a lSibs focused rSibs) = Branch a lSibs t (focused:rSibs)

-- | Insert a subtree "right" of the last focused child and focus it.
tInsRight :: Tree a -> Tree a -> Tree a
tInsRight t (Leaf a) = Branch a [] t []
tInsRight t (Branch a lSibs focused rSibs) = Branch a (focused:lSibs) t rSibs

-- | Delete the focused child of a tree, if possible.
tDelete :: Tree a -> Maybe (Tree a)
tDelete (Leaf _) = Nothing
tDelete (Branch a [] _ []) = Just (Leaf a)
tDelete (Branch a (l:lSibs) _ []) = Just (Branch a lSibs l [])
tDelete (Branch a lSibs _ (r:rSibs)) = Just (Branch a lSibs r rSibs)

-- | Focus the leftmost child of a tree, if possible.
tLeftmost :: Tree a -> Maybe (Tree a)
tLeftmost (Leaf _) = Nothing
tLeftmost (Branch _ [] _ _) = Nothing
tLeftmost (Branch a lSibs focused rSibs) =
  let focused' = last lSibs
      rSibs' = ((reverse . init) lSibs) ++ [focused] ++ rSibs
  in  Just (Branch a [] focused' rSibs')

-- | Focus the rightmost child of a tree, if possible.
tRightmost :: Tree a -> Maybe (Tree a)
tRightmost (Leaf _) = Nothing
tRightmost (Branch _ _ _ []) = Nothing
tRightmost (Branch a lSibs focused rSibs) =
  let focused' = last rSibs
      lSibs' = ((reverse . init) rSibs) ++ [focused] ++ lSibs
  in  Just (Branch a lSibs' focused' [])

-- | Sort the children of a tree, given a ordering function.
tSortOn :: (Ord b) => (Tree a -> b) -> Tree a -> Tree a
tSortOn f (Leaf a) = Leaf a
tSortOn f (Branch a lSibs focused rSibs) =
  let children = L.sortOn f ((reverse lSibs) ++ rSibs)
      lSibs' = (reverse . takeWhile ((f focused >) . f)) children
      rSibs' = drop (length lSibs') children
  in  Branch a lSibs' focused rSibs'

-- | Reverse the children of a tree.
tReverse :: Tree a -> Tree a
tReverse (Leaf a) = Leaf a
tReverse (Branch a lSibs focused rSibs) = Branch a rSibs focused lSibs
