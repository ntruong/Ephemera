module UI.Render
  ( render
  ) where

import qualified Brick.Types as B (Widget)
import Core.Types (Mode(..), State(..))
import qualified UI.Modes.Help as H
import qualified UI.Modes.Normal as N
import qualified UI.Modes.Edit as P

render :: State -> [B.Widget ()]
render s =
  let render' = case mode s of
        Help -> H.render
        Normal -> N.render
        (Edit _ _) -> P.render
  in render' s
