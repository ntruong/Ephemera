module Core.Zipper
  ( Zipper(..)
  , Ctx(..)
  , root
  , up
  , down
  , left
  , leftmost
  , right
  , rightmost
  , delete
  , append
  , prepend
  , top
  , sortOn
  , list
  ) where

import qualified Data.List as L (sortOn)

import Core.Focused (Focused(..))
import Core.Tree (Tree(..))

-- | Context saves the state of the rest of the tree, relative to the focus.
-- Siblings are stored in increasing distance from the focus, i.e. sibling
-- indices increase relative to distance.
data Ctx a = Root | Path (Ctx a) a [Tree a] [Tree a]

-- | Zippers store the current focused tree along with its surrounding context.
data Zipper a = Zipper (Tree a) (Ctx a)

instance Focused Zipper where
  f <.> (Zipper focused ctx) = Zipper (f <.> focused) ctx
  extract (Zipper focused _) = extract focused

-- | Get the focused tree of the zipper.
root :: Zipper a -> Tree a
root (Zipper focused _) = focused

-- | Move up from the focus.
up :: Zipper a -> Zipper a
up z@(Zipper _ Root) = z
up (Zipper focused (Path path a lSibs rSibs)) =
  Zipper (Branch a lSibs focused rSibs) path

-- | Move down from the focus.
down :: Zipper a -> Zipper a
down z@(Zipper (Leaf _) _) = z
down (Zipper (Branch a lSibs focused rSibs) path) =
  Zipper focused (Path path a lSibs rSibs)

-- | Move left of the focus.
left :: Zipper a -> Zipper a
left z@(Zipper _ Root) = z
left z@(Zipper _ (Path _ _ [] _)) = z
left (Zipper focused (Path path a (l:lSibs) rSibs)) =
  Zipper l (Path path a lSibs (focused:rSibs))

-- | Move to the leftmost element of the focus.
leftmost :: Zipper a -> Zipper a
leftmost z@(Zipper _ Root) = z
leftmost z@(Zipper _ (Path _ _ [] _)) = z
leftmost z = (leftmost . left) z

-- | Move right of the focus.
right :: Zipper a -> Zipper a
right z@(Zipper _ Root) = z
right z@(Zipper _ (Path _ _ _ [])) = z
right (Zipper focused (Path path a lSibs (r:rSibs))) =
  Zipper r (Path path a (focused:lSibs) rSibs)

-- | Move to the rightmost element of the focus.
rightmost :: Zipper a -> Zipper a
rightmost z@(Zipper _ Root) = z
rightmost z@(Zipper _ (Path _ _ _ [])) = z
rightmost z = (rightmost . right) z

-- | Delete the currently focused element. Cannot delete root.
delete :: Zipper a -> Zipper a
delete z@(Zipper _ Root) = z
delete (Zipper _ (Path ctx a [] [])) = Zipper (Leaf a) ctx
delete (Zipper _ (Path ctx a (focused:lSibs) [])) =
  Zipper focused (Path ctx a lSibs [])
delete (Zipper _ (Path ctx a lSibs (focused:rSibs))) =
  Zipper focused (Path ctx a lSibs rSibs)

-- | Append a tree.
append :: Tree a -> Zipper a -> Zipper a
append _ z@(Zipper _ Root) = z
append tree (Zipper focused (Path ctx a lSibs rSibs)) =
  Zipper focused (Path ctx a lSibs (tree:rSibs))

-- | Prepend a tree.
prepend :: Tree a -> Zipper a -> Zipper a
prepend _ z@(Zipper _ Root) = z
prepend tree (Zipper focused (Path ctx a lSibs rSibs)) =
  Zipper focused (Path ctx a (tree:lSibs) rSibs)

-- | Move to the root of the zipper.
top :: Zipper a -> Zipper a
top z@(Zipper _ Root) = z
top z = (top . up) z

-- | Sort the current focus and its siblings given an ordering function.
sortOn :: Ord b => (Tree a -> b) -> Zipper a -> Zipper a
sortOn f z@(Zipper _ Root) = z
sortOn f (Zipper focused (Path ctx a lSibs rSibs)) =
  let siblings = L.sortOn f (reverse lSibs ++ rSibs)
      lSibs' = (reverse . takeWhile ((f focused >) . f)) siblings
      rSibs' = drop (length lSibs') siblings
  in  Zipper focused (Path ctx a lSibs' rSibs')

-- | Get (sub)zippers matching a given filtering function.
list :: Zipper a -> [Zipper a]
list z@(Zipper t@(Leaf _) _) = [z]
list z@(Zipper t@(Branch a lSibs focused rSibs) ctx) =
  let makeZip (t, l, r) = Zipper t (Path ctx a l r)
      splitAround (i, xs) =
        let (beg, end) = splitAt i xs
        in  (last beg, (reverse . init) beg, end)
      children = reverse lSibs ++ [focused] ++ rSibs
      childZips = makeZip . splitAround
        <$> zip [1..(length children)] (repeat children)
  in  z : concat (list <$> childZips)
