{-# LANGUAGE OverloadedStrings #-}
module IO.Data
  ( decode
  , encode
  ) where

import qualified Data.ByteString.Lazy as BS (readFile, writeFile)
import qualified Data.Aeson as A (decode, encode)
import Core.Tree (Tree(Leaf))
import Core.Types (Note, empty)
import Core.Zipper (Ctx(Root), Zipper(..))
import IO.Types

-- | Decode a binary to restore the state.
decode :: IO (Zipper Note)
decode = do
  file <- BS.readFile "ephemera.bin"
  let z = case A.decode file of
        Just z' -> z'
        Nothing -> Zipper (Leaf empty) Root
  return z

-- | Encode a zipper to save the state.
encode :: Zipper Note -> IO ()
encode = BS.writeFile "ephemera.bin" . A.encode
