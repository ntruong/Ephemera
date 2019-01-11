module UI.Handle
  ( handle
  ) where

import qualified Brick.Types as B (BrickEvent, EventM, Next)
import Core.Types (Mode(..), State(..))
import qualified UI.Modes.Help as H
import qualified UI.Modes.Normal as N
import qualified UI.Modes.Pending as P

handle :: State -> B.BrickEvent () e -> B.EventM () (B.Next State)
handle s@(State _ Help _) = H.handle s
handle s@(State _ Normal _) = N.handle s
handle s@(State _ (Pending _ _) _) = P.handle s
