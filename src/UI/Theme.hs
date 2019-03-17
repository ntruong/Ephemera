module UI.Theme (theme) where

import Brick.AttrMap (attrName)
import Brick.Themes (Theme, newTheme)
import Brick.Util (fg)
import Graphics.Vty.Attributes
  ( black
  , red
  , green
  , yellow
  , blue
  , magenta
  , cyan
  , white
  )

-- | The theme of the application. TODO(ntruong) make this configurable in the
-- config.ini file.
theme :: Theme
theme =
  let attrs = [ (attrName "title",     fg cyan)
              , (attrName "special",   fg magenta)
              , (attrName "focused",   fg magenta)
              , (attrName "unfocused", fg white)
              , (attrName "progress",  fg yellow)
              , (attrName "low",       fg green)
              , (attrName "mid",       fg yellow)
              , (attrName "high",      fg red)
              ]
  in newTheme (fg white) attrs
