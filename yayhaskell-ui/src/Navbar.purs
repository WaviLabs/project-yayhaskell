module Navbar where

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
import Halogen.VDom.Driver (runUI)
import Navbar.Switch as Switch

-- Code For SVG Icons --
import Svg.Renderer.Halogen (icon)

logo1   = """<svg aria-hidden="true" focusable="false" data-prefix="fad" data-icon="angle-double-right" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 448 512" class="svg-inline--fa fa-angle-double-right fa-w-14 fa-5x" > <g class="fa-group"> <path fill="currentColor" d="M224 273L88.37 409a23.78 23.78 0 0 1-33.8 0L32 386.36a23.94 23.94 0 0 1 0-33.89l96.13-96.37L32 159.73a23.94 23.94 0 0 1 0-33.89l22.44-22.79a23.78 23.78 0 0 1 33.8 0L223.88 239a23.94 23.94 0 0 1 .1 34z" class="fa-secondary" ></path> <path fill="currentColor" d="M415.89 273L280.34 409a23.77 23.77 0 0 1-33.79 0L224 386.26a23.94 23.94 0 0 1 0-33.89L320.11 256l-96-96.47a23.94 23.94 0 0 1 0-33.89l22.52-22.59a23.77 23.77 0 0 1 33.79 0L416 239a24 24 0 0 1-.11 34z" class="fa-primary" ></path> </g> </svg> </a></li><li class="nav-item"> <a href="#" class="nav-link"> <svg aria-hidden="true" focusable="false" data-prefix="fad" data-icon="cat" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512" class="svg-inline--fa fa-cat fa-w-16 fa-9x" > <g class="fa-group"> <path fill="currentColor" d="M448 96h-64l-64-64v134.4a96 96 0 0 0 192 0V32zm-72 80a16 16 0 1 1 16-16 16 16 0 0 1-16 16zm80 0a16 16 0 1 1 16-16 16 16 0 0 1-16 16zm-165.41 16a204.07 204.07 0 0 0-34.59 2.89V272l-43.15-64.73a183.93 183.93 0 0 0-44.37 26.17L192 304l-60.94-30.47L128 272v-80a96.1 96.1 0 0 0-96-96 32 32 0 0 0 0 64 32 32 0 0 1 32 32v256a64.06 64.06 0 0 0 64 64h176a16 16 0 0 0 16-16v-16a32 32 0 0 0-32-32h-32l128-96v144a16 16 0 0 0 16 16h32a16 16 0 0 0 16-16V289.86a126.78 126.78 0 0 1-32 4.54c-61.81 0-113.52-44.05-125.41-102.4z" class="fa-secondary" ></path> <path fill="currentColor" d="M376 144a16 16 0 1 0 16 16 16 16 0 0 0-16-16zm80 0a16 16 0 1 0 16 16 16 16 0 0 0-16-16zM131.06 273.53L192 304l-23.52-70.56a192.06 192.06 0 0 0-37.42 40.09zM256 272v-77.11a198.62 198.62 0 0 0-43.15 12.38z" class="fa-primary" ></path> </g> </svg>"""
sunSvg  = """<svg xmlns="http://www.w3.org/2000/svg" aria-hidden="true" focusable="false" data-prefix="fas" data-icon="sun" class="svg-inline--fa fa-sun fa-w-16" role="img" viewBox="0 0 512 512"><path fill="currentColor" d="M256 160c-52.9 0-96 43.1-96 96s43.1 96 96 96 96-43.1 96-96-43.1-96-96-96zm246.4 80.5l-94.7-47.3 33.5-100.4c4.5-13.6-8.4-26.5-21.9-21.9l-100.4 33.5-47.4-94.8c-6.4-12.8-24.6-12.8-31 0l-47.3 94.7L92.7 70.8c-13.6-4.5-26.5 8.4-21.9 21.9l33.5 100.4-94.7 47.4c-12.8 6.4-12.8 24.6 0 31l94.7 47.3-33.5 100.5c-4.5 13.6 8.4 26.5 21.9 21.9l100.4-33.5 47.3 94.7c6.4 12.8 24.6 12.8 31 0l47.3-94.7 100.4 33.5c13.6 4.5 26.5-8.4 21.9-21.9l-33.5-100.4 94.7-47.3c13-6.5 13-24.7.2-31.1zm-155.9 106c-49.9 49.9-131.1 49.9-181 0-49.9-49.9-49.9-131.1 0-181 49.9-49.9 131.1-49.9 181 0 49.9 49.9 49.9 131.1 0 181z"/></svg>"""
moonSvg = """<svg xmlns="http://www.w3.org/2000/svg" aria-hidden="true" focusable="false" data-prefix="fas" data-icon="moon" class="svg-inline--fa fa-moon fa-w-16" role="img" viewBox="0 0 512 512"><path fill="currentColor" d="M283.211 512c78.962 0 151.079-35.925 198.857-94.792 7.068-8.708-.639-21.43-11.562-19.35-124.203 23.654-238.262-71.576-238.262-196.954 0-72.222 38.662-138.635 101.498-174.394 9.686-5.512 7.25-20.197-3.756-22.23A258.156 258.156 0 0 0 283.211 0c-141.309 0-256 114.511-256 256 0 141.309 114.511 256 256 256z"/></svg>"""


type Icon = forall p r i. Array (IProp r i) -> HTML p i

-- | For applying a class attribute to an icon.
className :: forall r i. String -> HH.IProp r i
className = HH.attr (HH.AttrName "class")

data Action
  = HandleSwitch Switch.Message
  | CheckButtonState

type State = { switchState :: Maybe Switch.State }

type ChildSlots =
  ( switch :: Switch.Slot Unit
  )

_switch :: SProxy "switch"
_switch = SProxy

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = { switchState: Just $ { isLight: true, lightSvg: sunSvg, darkSvg: moonSvg } }

render :: forall m. State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.nav
    [ HP.class_ $ ClassName "navbar" ]
    [ HH.ul
        [ HP.class_ $ ClassName "navbar-nav" ]
        ( renderListHeader logo1 : map renderListItem
            -- SVG goes here
            [ Tuple logo1 "Home"
            , Tuple logo1 "News"
            , Tuple logo1 "Blog"
            -- TODO: Change to switch button
            ]
            <>
            [ HH.li
                [HP.class_ $ ClassName "nav-item"]
                [HH.slot _switch unit Switch.component unit (Just <<< HandleSwitch)]
            ]
        )
    ]

handleAction ::forall o m. Action -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  HandleSwitch (Switch.Toggled _) -> do
    pure unit
  CheckButtonState -> do
    switchState <- H.query _switch unit $ H.request Switch.IsLight
    H.modify_ (_ { switchState = switchState })

renderListHeader x =
    HH.li
        [HP.class_ $ ClassName "logo"]
        [ HH.a
            [ HP.class_ $ ClassName "nav-link"
            , HP.href "#"
            ]
            [ HH.span
                [HP.classes [ClassName "link-text", ClassName "logo-text"]]
                [HH.text "Yay"]
            , icon x
                [className "icon"]
            ]
        ]

renderListItem (Tuple x y) =
    HH.li
        [HP.class_ $ ClassName "nav-item"]
        [ HH.a
            [ HP.class_ $ ClassName "nav-link"
            , HP.href "#"
            ]
            [ icon x
                [className "icon"]
            , HH.span
                [HP.class_ $ ClassName "link-text"]
                [HH.text y]
            ]
        ]
