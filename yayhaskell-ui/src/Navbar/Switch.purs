module Navbar.Switch where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML (ClassName(..), HTML, IProp)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- Code For SVG Icons --
import Svg.Renderer.Halogen (icon)

type Icon = forall p r i. Array (IProp r i) -> HTML p i

sunSvg  = """<svg xmlns="http://www.w3.org/2000/svg" aria-hidden="true" focusable="false" data-prefix="fas" data-icon="sun" class="svg-inline--fa fa-sun fa-w-16" role="img" viewBox="0 0 512 512"><path fill="currentColor" d="M256 160c-52.9 0-96 43.1-96 96s43.1 96 96 96 96-43.1 96-96-43.1-96-96-96zm246.4 80.5l-94.7-47.3 33.5-100.4c4.5-13.6-8.4-26.5-21.9-21.9l-100.4 33.5-47.4-94.8c-6.4-12.8-24.6-12.8-31 0l-47.3 94.7L92.7 70.8c-13.6-4.5-26.5 8.4-21.9 21.9l33.5 100.4-94.7 47.4c-12.8 6.4-12.8 24.6 0 31l94.7 47.3-33.5 100.5c-4.5 13.6 8.4 26.5 21.9 21.9l100.4-33.5 47.3 94.7c6.4 12.8 24.6 12.8 31 0l47.3-94.7 100.4 33.5c13.6 4.5 26.5-8.4 21.9-21.9l-33.5-100.4 94.7-47.3c13-6.5 13-24.7.2-31.1zm-155.9 106c-49.9 49.9-131.1 49.9-181 0-49.9-49.9-49.9-131.1 0-181 49.9-49.9 131.1-49.9 181 0 49.9 49.9 49.9 131.1 0 181z"/></svg>"""
moonSvg = """<svg xmlns="http://www.w3.org/2000/svg" aria-hidden="true" focusable="false" data-prefix="fas" data-icon="moon" class="svg-inline--fa fa-moon fa-w-16" role="img" viewBox="0 0 512 512"><path fill="currentColor" d="M283.211 512c78.962 0 151.079-35.925 198.857-94.792 7.068-8.708-.639-21.43-11.562-19.35-124.203 23.654-238.262-71.576-238.262-196.954 0-72.222 38.662-138.635 101.498-174.394 9.686-5.512 7.25-20.197-3.756-22.23A258.156 258.156 0 0 0 283.211 0c-141.309 0-256 114.511-256 256 0 141.309 114.511 256 256 256z"/></svg>"""

-- | For applying a class attribute to an icon.
className :: forall r i. String -> HH.IProp r i
className = HH.attr (HH.AttrName "class")

data Query a = Const Void

data Message = Toggled

data Action = Toggle

type State = { isLight  :: Boolean
             , lightSvg :: String
             , darkSvg  :: String
             }

component :: forall i m. H.Component HH.HTML Query i Message m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
    }

initialState :: forall i. i -> State
initialState _ = { isLight: true, lightSvg: sunSvg, darkSvg: moonSvg }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  let
    modeIcon = if state.isLight
      then icon state.darkSvg [className "icon"]
      else icon state.lightSvg [className "icon"]

    modeText = if state.isLight
      then "Dark Mode"
      else "Light Mode"
  in
    HH.a
      [ HP.class_ $ ClassName "nav-link"
      , HP.href "#"
      , HE.onClick \_ -> Just Toggle
      ]
      [ modeIcon
      , HH.span
          [HP.class_ $ ClassName "link-text"]
          [HH.text modeText]
      ]

handleAction :: forall m. Action -> H.HalogenM State Action () Message m Unit
handleAction = case _ of
  Toggle -> do
    newState <- H.modify \st -> st { isLight = not st.isLight }
    H.raise Toggled
