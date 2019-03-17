module Core.Series
  ( Series(..)
  , (>>.)
  , backwards
  , forwards
  , new
  ) where

import Core.Focused (Focused(..))
import Core.Serial (Serial(..))

-- | Represent arbitrary data at some point in time.
data Series a = Series a (Maybe (Series a)) (Maybe (Series a))

instance Focused Series where
  f <.> (Series a prev next) = Series (f a) prev next
  extract (Series a _ _) = a

instance Serial Series where
  serialize a (b:bs) = Series a Nothing (Just next)
    where
      (Series a' _ next') = serialize b bs
      next = Series a' (Just $ serialize a []) next'
  serialize a [] = Series a Nothing Nothing

  deserialize (Series a Nothing Nothing) = [a]
  deserialize (Series a Nothing (Just next)) = a : deserialize next
  deserialize (Series a (Just prev) Nothing) = deserialize prev ++ [a]
  deserialize (Series a (Just prev) (Just next)) = left ++ [a] ++ right
    where
      left  = deserialize prev
      right = deserialize next

-- | Given new data, advance the series and update pointers.
(>>.) :: (a -> a) -> Series a -> Series a
f >>. series@(Series a _ _) = Series (f a) (Just series) Nothing

-- | Move backward in the series if possible.
backwards :: Series a -> Series a
backwards next@(Series _ (Just (Series a prev _)) _) = Series a prev (Just next)
backwards series = series

-- | Move forward in the series if possible.
forwards :: Series a -> Series a
forwards series@(Series _ _ (Just next)) = series'
  where
    (Series a _ next') =  next
    series' = Series a (Just series) next'
forwards series = series

-- | Create a new series from data.
new :: a -> Series a
new a = Series a Nothing Nothing
