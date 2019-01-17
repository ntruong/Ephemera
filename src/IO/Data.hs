{-# LANGUAGE OverloadedStrings #-}
module IO.Data
  ( decode
  , encode
  ) where

import qualified Data.ByteString.Lazy as BS (readFile, writeFile)
import qualified Data.Aeson as A (decode, encode)
import qualified Data.Maybe as M (fromMaybe)
import qualified System.Directory as D (doesFileExist)
import Core.Tree (Tree(Leaf))
import Core.Types (Note, empty)
import Core.Zipper (Ctx(Root), Zipper(..))
import IO.Types

-- | Decode a binary to restore the state.
decode :: IO (Zipper Note)
decode = do
  exists <- D.doesFileExist "ephemera.bin"
  if exists
    then do
      file <- BS.readFile "ephemera.bin"
      return $ M.fromMaybe (Zipper (Leaf empty) Root) (A.decode file)
      -- return $ case A.decode file of
      --   Just z -> z
      --   Nothing -> Zipper (Leaf empty) Root
    else return (Zipper (Leaf empty) Root)

-- | Encode a zipper to save the state.
encode :: Zipper Note -> IO ()
encode = BS.writeFile "ephemera.bin" . A.encode
