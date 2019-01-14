module UI.Theme
  ( theme
  ) where

import qualified Brick.AttrMap as B (attrName)
import qualified Brick.Themes as B (Theme, newTheme)
import qualified Brick.Util as B (fg)
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
  let attrs = [ (B.attrName "title",     B.fg V.cyan)
              , (B.attrName "special",   B.fg V.magenta)
              , (B.attrName "focused",   B.fg V.magenta)
              , (B.attrName "unfocused", B.fg V.white)
              , (B.attrName "progress",  B.fg V.yellow)
              , (B.attrName "low",       B.fg V.green)
              , (B.attrName "mid",       B.fg V.yellow)
              , (B.attrName "high",      B.fg V.red)
              ]
  in B.newTheme (B.fg V.white) attrs
