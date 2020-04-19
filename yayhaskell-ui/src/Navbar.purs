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
import Effect.Class (liftEffect, class MonadEffect)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Icon as Icon
import Navbar.Switch as Switch

import Effect.Console as Console

logo       = """<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" x="0px" y="0px" viewBox="0 0 1000 1000" enable-background="new 0 0 1000 1000" xml:space="preserve">
<g><path d="M413.5,154.1l461.2,691.8H701.8L557.7,629.7L413.5,845.9H240.6L471.2,500L240.6,154.1H413.5z M182.9,154.1H10L240.6,500L10,845.9h172.9L413.5,500L182.9,154.1z M605.7,355.9l76.9,115.3H990V355.9L605.7,355.9z M797.8,644.1H990V528.8l-269,0L797.8,644.1z"/></g>
</svg>"""
homeIcon   = """<svg xmlns="http://www.w3.org/2000/svg" aria-hidden="true" focusable="false" data-prefix="fas" data-icon="home" class="svg-inline--fa fa-home fa-w-18" role="img" viewBox="0 0 576 512"><path fill="currentColor" d="M280.37 148.26L96 300.11V464a16 16 0 0 0 16 16l112.06-.29a16 16 0 0 0 15.92-16V368a16 16 0 0 1 16-16h64a16 16 0 0 1 16 16v95.64a16 16 0 0 0 16 16.05L464 480a16 16 0 0 0 16-16V300L295.67 148.26a12.19 12.19 0 0 0-15.3 0zM571.6 251.47L488 182.56V44.05a12 12 0 0 0-12-12h-56a12 12 0 0 0-12 12v72.61L318.47 43a48 48 0 0 0-61 0L4.34 251.47a12 12 0 0 0-1.6 16.9l25.5 31A12 12 0 0 0 45.15 301l235.22-193.74a12.19 12.19 0 0 1 15.3 0L530.9 301a12 12 0 0 0 16.9-1.6l25.5-31a12 12 0 0 0-1.7-16.93z"/></svg>"""
profIcon   = """<svg xmlns="http://www.w3.org/2000/svg" aria-hidden="true" focusable="false" data-prefix="fas" data-icon="user" class="svg-inline--fa fa-user fa-w-14" role="img" viewBox="0 0 448 512"><path fill="currentColor" d="M224 256c70.7 0 128-57.3 128-128S294.7 0 224 0 96 57.3 96 128s57.3 128 128 128zm89.6 32h-16.7c-22.2 10.2-46.9 16-72.9 16s-50.6-5.8-72.9-16h-16.7C60.2 288 0 348.2 0 422.4V464c0 26.5 21.5 48 48 48h352c26.5 0 48-21.5 48-48v-41.6c0-74.2-60.2-134.4-134.4-134.4z"/></svg>"""
postIcon   = """<svg xmlns="http://www.w3.org/2000/svg" aria-hidden="true" focusable="false" data-prefix="fas" data-icon="pen" class="svg-inline--fa fa-pen fa-w-16" role="img" viewBox="0 0 512 512"><path fill="currentColor" d="M290.74 93.24l128.02 128.02-277.99 277.99-114.14 12.6C11.35 513.54-1.56 500.62.14 485.34l12.7-114.22 277.9-277.88zm207.2-19.06l-60.11-60.11c-18.75-18.75-49.16-18.75-67.91 0l-56.55 56.55 128.02 128.02 56.55-56.55c18.75-18.76 18.75-49.16 0-67.91z"/></svg>"""
helpIcon   = """<svg xmlns="http://www.w3.org/2000/svg" aria-hidden="true" focusable="false" data-prefix="fas" data-icon="question" class="svg-inline--fa fa-question fa-w-12" role="img" viewBox="0 0 384 512"><path fill="currentColor" d="M202.021 0C122.202 0 70.503 32.703 29.914 91.026c-7.363 10.58-5.093 25.086 5.178 32.874l43.138 32.709c10.373 7.865 25.132 6.026 33.253-4.148 25.049-31.381 43.63-49.449 82.757-49.449 30.764 0 68.816 19.799 68.816 49.631 0 22.552-18.617 34.134-48.993 51.164-35.423 19.86-82.299 44.576-82.299 106.405V320c0 13.255 10.745 24 24 24h72.471c13.255 0 24-10.745 24-24v-5.773c0-42.86 125.268-44.645 125.268-160.627C377.504 66.256 286.902 0 202.021 0zM192 373.459c-38.196 0-69.271 31.075-69.271 69.271 0 38.195 31.075 69.27 69.271 69.27s69.271-31.075 69.271-69.271-31.075-69.27-69.271-69.27z"/></svg>"""
sunSvg     = """<svg xmlns="http://www.w3.org/2000/svg" aria-hidden="true" focusable="false" data-prefix="fas" data-icon="sun" class="svg-inline--fa fa-sun fa-w-16" role="img" viewBox="0 0 512 512"><path fill="currentColor" d="M256 160c-52.9 0-96 43.1-96 96s43.1 96 96 96 96-43.1 96-96-43.1-96-96-96zm246.4 80.5l-94.7-47.3 33.5-100.4c4.5-13.6-8.4-26.5-21.9-21.9l-100.4 33.5-47.4-94.8c-6.4-12.8-24.6-12.8-31 0l-47.3 94.7L92.7 70.8c-13.6-4.5-26.5 8.4-21.9 21.9l33.5 100.4-94.7 47.4c-12.8 6.4-12.8 24.6 0 31l94.7 47.3-33.5 100.5c-4.5 13.6 8.4 26.5 21.9 21.9l100.4-33.5 47.3 94.7c6.4 12.8 24.6 12.8 31 0l47.3-94.7 100.4 33.5c13.6 4.5 26.5-8.4 21.9-21.9l-33.5-100.4 94.7-47.3c13-6.5 13-24.7.2-31.1zm-155.9 106c-49.9 49.9-131.1 49.9-181 0-49.9-49.9-49.9-131.1 0-181 49.9-49.9 131.1-49.9 181 0 49.9 49.9 49.9 131.1 0 181z"/></svg>"""
moonSvg    = """<svg xmlns="http://www.w3.org/2000/svg" aria-hidden="true" focusable="false" data-prefix="fas" data-icon="moon" class="svg-inline--fa fa-moon fa-w-16" role="img" viewBox="0 0 512 512"><path fill="currentColor" d="M283.211 512c78.962 0 151.079-35.925 198.857-94.792 7.068-8.708-.639-21.43-11.562-19.35-124.203 23.654-238.262-71.576-238.262-196.954 0-72.222 38.662-138.635 101.498-174.394 9.686-5.512 7.25-20.197-3.756-22.23A258.156 258.156 0 0 0 283.211 0c-141.309 0-256 114.511-256 256 0 141.309 114.511 256 256 256z"/></svg>"""

data Action
  = HandleSwitch Switch.Message

type State = { switchState :: Maybe Switch.State }

data Message = ViewModeToggled

type ChildSlots =
  ( switch :: H.Slot Switch.Query Switch.Message Int
  )

_switch :: SProxy "switch"
_switch = SProxy

component :: forall q i m. MonadEffect m => H.Component HH.HTML q i Message m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = { switchState: Just $ { isLight: true, lightSvg: sunSvg, darkSvg: moonSvg } }

render :: forall m. MonadEffect m => State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.nav
    [ HP.class_ $ ClassName "navbar" ]
    [ HH.ul
        [ HP.class_ $ ClassName "navbar-nav" ]
        ( renderListHeader logo : map renderListItem
            -- SVG goes here
            [ Tuple homeIcon "Home"
            , Tuple profIcon "Profile"
            , Tuple postIcon "Post"
            , Tuple helpIcon "Help"
            ]
            <>
            [ HH.li
                [HP.class_ $ ClassName "nav-item"]
                [HH.slot _switch 1 Switch.component unit (\msg -> Just $ HandleSwitch msg)]
            ]
        )
    ]

handleAction ::forall m. MonadEffect m => Action -> H.HalogenM State Action ChildSlots Message m Unit
handleAction = case _ of
  HandleSwitch _ -> do
    liftEffect $ Console.log "Hello from navbar. Talking to main."
    H.raise ViewModeToggled

renderListHeader x =
    HH.li
        [HP.class_ $ ClassName "logo"]
        [ HH.a
            [ HP.class_ $ ClassName "nav-link"
            , HP.href "#"
            ]
            [ HH.span
                [HP.classes [ClassName "link-text", ClassName "logo-text"]]
                [HH.text "YAYHASKELL"]
            , Icon.mkIcon x
                [Icon.className "icon"]
            ]
        ]

renderListItem (Tuple x y) =
    HH.li
        [HP.class_ $ ClassName "nav-item"]
        [ HH.a
            [ HP.class_ $ ClassName "nav-link"
            , HP.href "#"
            ]
            [ Icon.mkIcon x
                [Icon.className "icon"]
            , HH.span
                [HP.class_ $ ClassName "link-text"]
                [HH.text y]
            ]
        ]
