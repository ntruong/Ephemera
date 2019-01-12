{-# LANGUAGE TemplateHaskell #-}
module IO.Types where

import qualified Data.Aeson.TH as A (defaultOptions, deriveJSON)
import Core.Tree (Tree)
import Core.Types (Note, Priority)
import Core.Zipper (Ctx, Zipper)

-- | JSON derivation for main types.
$(A.deriveJSON A.defaultOptions ''Priority)
$(A.deriveJSON A.defaultOptions ''Note)
$(A.deriveJSON A.defaultOptions ''Tree)
$(A.deriveJSON A.defaultOptions ''Ctx)
$(A.deriveJSON A.defaultOptions ''Zipper)
