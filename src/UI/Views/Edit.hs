module UI.Views.Edit (render) where

import Core.Focused (Focused(..))
import Core.Types (Note, Resource, View, editName, editDesc, editDate)
import qualified UI.Views.Normal as Normal (render)

render :: (Note -> Note) -> View
render f notes = Normal.render $ (f <.>) <.> notes
