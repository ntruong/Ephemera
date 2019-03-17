module Core.Serial (Serial(..)) where

-- | Type class for data structures that can be converted to and from lists.
class Serial f where
  -- | Convert a list to f.
  serialize :: a -> [a] -> f a
  -- | Convert f to a list.
  deserialize :: f a -> [a]
