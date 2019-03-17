module UI.App
  ( render
  , handle
  ) where

import Brick.Types (BrickEvent, EventM, Next, Widget)

import Core.Types (Handler(..), Resource, State)

-- | Main rendering function for the app; simply select the appropriate renderer
-- given in the state.
render :: State -> [Widget Resource]
render (state, view, _) = view state

-- | Main handler function for the app; simply select the appropriate handler
-- given in the state.
handle :: State -> BrickEvent Resource () -> EventM Resource (Next State)
handle (state, _, Handler handler) = handler state
