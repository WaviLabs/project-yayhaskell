module ToggleViewMode where

import Data.Unit (Unit)
import Effect (Effect)

foreign import toggleViewMode :: Effect Unit
