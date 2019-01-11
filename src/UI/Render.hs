module UI.Render
  ( render
  ) where

import qualified Brick.Types as B (Widget)
import Core.Types (Mode(..), State(..))
import qualified UI.Modes.Help as H
import qualified UI.Modes.Normal as N
import qualified UI.Modes.Pending as P

render :: State -> [B.Widget ()]
render s@(State _ Help _) = H.render s
render s@(State _ Normal _) = N.render s
render s@(State _ (Pending _ _) _) = P.render s
