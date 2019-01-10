module Core.Zipper
  ( Zipper(..)
  , Ctx(..)
  , focus
  , up
  , down
  , left
  , right
  , modify
  , modifyA
  , modifyM
  ) where

import Core.Tree

-- | Context saves the state of the rest of the tree, relative to the focus.
-- Siblings are stored in increasing distance from the focus, i.e. sibling
-- indices increase relative to distance.
data Ctx a = Root | Path (Ctx a) a [Tree a] [Tree a]

-- | Zippers store the current focused tree along with its surrounding context.
data Zipper a = Zipper (Tree a) (Ctx a)

-- | Get the root of the focused tree of a given zipper.
focus :: Zipper a -> Tree a
focus (Zipper a _) = a

-- | Move "up" from the focus. Fails when the root element is focused.
up :: Zipper a -> Maybe (Zipper a)
up (Zipper _ Root) = Nothing
up (Zipper focused (Path path a lSibs rSibs)) =
  let z = Zipper (Branch a lSibs focused rSibs) path
  in  Just z

-- | Move "down" from the focus. Fails when a leaf element is focused.
down :: Zipper a -> Maybe (Zipper a)
down (Zipper (Leaf _) _) = Nothing
down (Zipper (Branch a lSibs focused rSibs) path) =
  let z = Zipper focused (Path path a lSibs rSibs)
  in  Just z

-- | Move "left" from the focus. Fails when the leftmost sibling is focused.
left :: Zipper a -> Maybe (Zipper a)
left (Zipper _ Root) = Nothing
left (Zipper _ (Path _ _ [] _)) = Nothing
left (Zipper focused (Path path a (l:lSibs) rSibs)) =
  let z = Zipper l (Path path a lSibs (focused:rSibs))
  in  Just z

-- | Move "right" from the focus. Fails when the rightmost sibling is focused.
right :: Zipper a -> Maybe (Zipper a)
right (Zipper _ Root) = Nothing
right (Zipper _ (Path _ _ _ [])) = Nothing
right (Zipper focused (Path path a lSibs (r:rSibs))) =
  let z = Zipper r (Path path a (focused:lSibs) rSibs)
  in  Just z

-- | Modify the focused tree.
modify :: (Tree a -> Tree a) -> Zipper a -> Zipper a
modify f (Zipper t ctx) = Zipper (f t) ctx

-- | Modify the focused element.
modifyA :: (a -> a) -> Zipper a -> Zipper a
modifyA f (Zipper (Leaf a) ctx) = Zipper (Leaf (f a)) ctx
modifyA f (Zipper (Branch a lSibs focused rSibs) ctx) =
  let t = Branch (f a) lSibs focused rSibs
  in  Zipper t ctx

-- | Modify the focused tree (with Maybe)
modifyM :: (Tree a -> Maybe (Tree a)) -> Zipper a -> Maybe (Zipper a)
modifyM f (Zipper t ctx) = case f t of
  Just t' -> Just (Zipper t' ctx)
  Nothing -> Nothing
