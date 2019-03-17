module Core.Focused (Focused(..)) where

-- | Type class for focused data structures.
class Focused f where
  -- | Modify just the focused element.
  (<.>) :: (a -> a) -> f a -> f a
  -- | Get the focused element.
  extract :: f a -> a
