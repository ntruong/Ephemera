module UI.Handle
  ( handle
  ) where

import qualified Brick.Types as B (BrickEvent, EventM, Next)
import Core.Types (Mode(..), State(..))
import qualified UI.Modes.Help as H
import qualified UI.Modes.Normal as N
import qualified UI.Modes.Edit as P

handle :: State -> B.BrickEvent () e -> B.EventM () (B.Next State)
handle s =
  let handle' = case mode s of
        Help -> H.handle
        Normal -> N.handle
        (Edit _ _) -> P.handle
  in handle' s
