module UI.Render
  ( render
  ) where

import qualified Brick.Types as B (Widget)
import Core.Types (Mode(..), Resource, State(..))
import qualified UI.Modes.Help as H
import qualified UI.Modes.Normal as N
import qualified UI.Modes.Edit as P

render :: State -> [B.Widget Resource]
render s =
  let render' = case mode s of
        Help -> H.render
        (Normal _) -> N.render
        (Edit _ _) -> P.render
  in render' s
