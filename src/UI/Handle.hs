module UI.Handle
  ( handle
  ) where

import qualified Brick.Types as B (BrickEvent, EventM, Next)
import Core.Types (Mode(..), Resource, State(..))
import qualified UI.Modes.Help as H
import qualified UI.Modes.List as L
import qualified UI.Modes.Normal as N
import qualified UI.Modes.Edit as P

handle :: State -> B.BrickEvent Resource e -> B.EventM Resource (B.Next State)
handle s =
  let handle' = case mode s of
        Help -> H.handle
        (List _ _) -> L.handle
        (Normal _) -> N.handle
        (Edit _ _) -> P.handle
  in handle' s
