module Core.FList
  ( FList(..)
  , left
  , right
  , focus
  , empty
  , list
  ) where

-- | Focused lists.
data FList a = FList [a] (Maybe a) [a]

-- | Move to the left element of the focused list.
left :: FList a -> FList a
left f@(FList [] m rs) = f
left (FList (l:ls) Nothing rs) = FList ls (Just l) rs
left (FList (l:ls) (Just m) rs) = FList ls (Just l) (m:rs)

-- | Move to the right element of the focused list.
right :: FList a -> FList a
right f@(FList ls m []) = f
right (FList ls Nothing (r:rs)) = FList ls (Just r) rs
right (FList ls (Just m) (r:rs)) = FList (m:ls) (Just r) rs

-- | Get the focused element if it exists.
focus :: FList a -> Maybe a
focus (FList _ m _) = m

-- | An empty flist.
empty :: FList a
empty = FList [] Nothing []

-- | Restore the focused list to a normal one.
list :: FList a -> [a]
list (FList ls Nothing rs) = reverse ls ++ rs
list (FList ls (Just m) rs) = reverse ls ++ [m] ++ rs
