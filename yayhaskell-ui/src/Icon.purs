module Icon (Icon(..), className, mkIcon) where

import Halogen.HTML (ClassName(..), HTML, IProp)
import Svg.Renderer.Halogen (icon)
import Halogen.HTML as HH

type Icon = forall p r i. Array (IProp r i) -> HTML p i

-- | For applying a class attribute to an icon.
className :: forall r i. String -> IProp r i
className = HH.attr (HH.AttrName "class")

mkIcon = icon
