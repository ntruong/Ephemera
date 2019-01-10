module UI.Theme
  ( theme
  ) where

import qualified Brick.AttrMap as B (attrName)
import qualified Brick.Themes as B (Theme, newTheme)
import qualified Brick.Util as B (fg, on)
import qualified Graphics.Vty.Attributes as V
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
theme :: B.Theme
theme =
  let attrs = [ (B.attrName "title",   B.fg V.cyan)
              , (B.attrName "special", B.fg V.magenta)
              , (B.attrName "status",  B.fg V.red)
              , (B.attrName "focus",   B.fg V.red)
              ]
  in B.newTheme (V.white `B.on` V.black) attrs
