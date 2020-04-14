module Script where

import Prelude

-- Imports for lesson
import Halogen.HTML (ClassName(..), HTML, IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties (ButtonType(..))
import Halogen.HTML.Properties as HP

-- Imports for scaffolding
import Data.Array ((:))
import Data.Const (Const)
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)

-- | HTML written in Purescript via Halogen's HTML DSL
-- | that is always rendered the same and does not include any event handling.
type StaticHTML = forall a b c. H.ComponentHTML a b c

-- | Shows how to use Halogen VDOM DSL to render HTML without properties or CSS
html :: StaticHTML
html =
  HH.script
    [ HP.src "bundle.js"
    ]
    []

-- | Wraps Halogen types cleanly, so that one gets very clear compiler errors
component :: StaticHTML
          -> H.Component HH.HTML (Const Void) Unit Void Aff
component renderHtml =
  H.mkComponent
    { initialState: const unit
    , render: \_ -> renderHtml
    , eval: H.mkEval H.defaultEval
    }
