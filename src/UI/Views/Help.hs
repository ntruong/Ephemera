module UI.Views.Help (render) where

import Brick.AttrMap (attrName)
import Brick.Types (Padding(Max), Widget)
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Core
  ( (<+>)
  , hLimit
  , padLeftRight
  , padRight
  , str
  , vBox
  , withAttr
  )

import Core.Types (Resource, View)

render :: View
render = const [menu]

doc :: [(String, String)]
doc =
  [ ("?"     , "Show this help menu" )
  , ("Esc"   , "Normal mode"         )
  , ("q"     , "Quit"                )
  , ("h"     , "Move left (higher)"  )
  , ("j"     , "Move down"           )
  , ("k"     , "Move up"             )
  , ("l"     , "Move right (lower)"  )
  , ("g"     , "Move to top"         )
  , ("G"     , "Move to bottom"      )
  , ("O"     , "Add task above"      )
  , ("o"     , "Add task below"      )
  , ("d"     , "Delete"              )
  , ("y"     , "Yank (additively)"   )
  , ("P"     , "Paste task above"    )
  , ("p"     , "Paste task below"    )
  , ("u"     , "Undo"                )
  , ("r"     , "Redo"                )
  , ("i"     , "Edit the name"       )
  , ("a"     , "Edit the description")
  , ("@"     , "Edit the date"       )
  , ("Space" , "Toggle status"       )
  , ("-"     , "Lower priority"      )
  , ("+"     , "Higher priority"     )
  , ("!"     , "Sort on date"        )
  , ("="     , "Sort on priority"    )
  , ("/"     , "Search"              )
  ]

menu :: Widget Resource
menu =
  let keys  = str . fst <$> doc
      binds = str . snd <$> doc
      label = withAttr (attrName "special") (str "Controls")
      text  =
        ( padRight Max
        . withAttr (attrName "special")
        . hLimit 8 . vBox
        ) keys
        <+> vBox binds
  in  ( center
      . hLimit 80
      . borderWithLabel label
      . padLeftRight 1
      ) text
