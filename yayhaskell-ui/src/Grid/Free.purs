module Grid.Free where

import Prelude

import Halogen.HTML (ClassName(..), HTML, IProp)
import Data.Const (Const)
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Halogen.HTML.Properties as HP

type Query = Const Void

type Input   = Void
type Message = Void

type Action = Void
-- | Shows how to use Halogen VDOM DSL to render HTML without properties or CSS
render :: StaticHTML
render =
  HH.div
    [ HP.class_ $ ClassName "grid-item"]
    -- The 'div' tag takes an Array of children
    [ HH.div_
      [ HH.span_
        -- as does the `span` tag
        [ HH.text "This is text in a span!" ]
      ]
    ]
-- | HTML written in Purescript via Halogen's HTML DSL
-- | that is always rendered the same and does not include any event handling.
type StaticHTML = forall m. H.ComponentHTML Unit () m

-- | Wraps Halogen types cleanly, so that one gets very clear compiler errors
component :: forall m. StaticHTML
          -> H.Component HH.HTML (Const Void) Unit Void m
component renderHtml =
  H.mkComponent
    { initialState: const unit
    , render: \_ -> renderHtml
    , eval: H.mkEval H.defaultEval
    }
