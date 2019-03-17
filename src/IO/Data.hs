{-# LANGUAGE OverloadedStrings #-}
module IO.Data
  ( decode
  , encode
  ) where

import qualified Data.Aeson as A (decode, encode)
import qualified Data.ByteString.Lazy as BS (readFile, writeFile)
import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist)
import Core.Tree (Tree(Leaf))
import Core.Types (Note, blank)
import Core.Zipper (Ctx(Root), Zipper(..))
import IO.Types

-- | Decode a binary to restore the state.
decode :: IO (Zipper Note)
decode = do
  exists <- doesFileExist "ephemera.bin"
  if exists
    then do
      file <- BS.readFile "ephemera.bin"
      return $ fromMaybe (Zipper (Leaf blank) Root) (A.decode file)
    else return (Zipper (Leaf blank) Root)

-- | Encode a zipper to save the state.
encode :: Zipper Note -> IO ()
encode = BS.writeFile "ephemera.bin" . A.encode
