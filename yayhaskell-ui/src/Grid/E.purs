module Grid.E where

import Prelude

import Halogen.HTML (ClassName(..), HTML, IProp)
import Data.Array (drop, length, take)
import Data.Const (Const)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Void (Void(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Icon as Icon

data Status = Rejected | Discussion | Accepted

derive instance eqStatus :: Eq Status

instance showStatus :: Show Status where
    show Rejected = "Rejected"
    show Discussion = "Discussion"
    show Accepted = "Accepted"

type State =
    { proposals   :: Array (Tuple String Status)
    , pageSize    :: Int
    , currentPage :: Int
    , lastPage    :: Int
    }

_button = SProxy :: SProxy "button"

type Query = Const Void

type Input   = Unit
type Message = Unit

data Action = PrevPage
            | NextPage

component :: forall f i o m. H.Component HH.HTML f Input Message m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
    }

testProps = [ Tuple "Dot Syntax" Accepted
            , Tuple "Dependent Types" Discussion
            , Tuple "Linear Types" Discussion
            , Tuple "Dynamic Types" Rejected
            , Tuple "Dot Syntax" Accepted
            , Tuple "Linear Types" Discussion
            , Tuple "Dynamic Types" Rejected
            , Tuple "Dot Syntax" Accepted
            , Tuple "Linear Types" Discussion
            , Tuple "Dynamic Types" Rejected
            , Tuple "Dynamic Types" Rejected
            , Tuple "Dot Syntax" Accepted
            , Tuple "Linear Types" Discussion
            , Tuple "Dynamic Types" Rejected
            , Tuple "Dot Syntax" Accepted
            , Tuple "Linear Types" Discussion
            , Tuple "Dynamic Types" Rejected
            , Tuple "Dynamic Types" Rejected
            , Tuple "Dot Syntax" Accepted
            , Tuple "Linear Types" Discussion
            , Tuple "Dynamic Types" Rejected
            , Tuple "Dot Syntax" Accepted
            , Tuple "Linear Types" Discussion
            , Tuple "Dynamic Types" Rejected
            , Tuple "Linear Types" Discussion
            , Tuple "Dynamic Types" Rejected
            , Tuple "Dot Syntax" Accepted
            , Tuple "Linear Types" Discussion
            , Tuple "Dynamic Types" Rejected
            , Tuple "Dot Syntax" Accepted
            , Tuple "Linear Types" Discussion
            , Tuple "Dynamic Types" Rejected
            , Tuple "Dynamic Types" Rejected
            , Tuple "Dot Syntax" Accepted
            , Tuple "Linear Types" Discussion
            ]

initialState :: forall i. i -> State
initialState _ =
    let
        proposals = testProps
        pageSize = 10
        currentPage = 1
        numOfProposals = length proposals
    in
        { proposals: proposals
        , pageSize: pageSize
        , currentPage: currentPage
        , lastPage: ((numOfProposals / pageSize) + (if numOfProposals `mod` pageSize == 0 then 0 else 1))
        }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
    HH.div
        [ HP.classes [ClassName "grid-item-tall"]
        ]
        [ HH.h1
            []
            [HH.text "GHC Proposals Status"]
        , HH.ul
            [HP.classes [ClassName "prop-status-list"]] $
                map renderStatusBar (getProposalsPage state) <>
                    [ HH.div
                        [HP.class_ $ ClassName "button-group"]
                        [ HH.button
                            [ HP.class_ $ ClassName "button"
                            , HE.onClick (\_ -> Just PrevPage)
                            ]
                            [Icon.mkIcon prevSvg []]
                        , HH.button
                            [ HP.class_ $ ClassName "button"
                            , HE.onClick (\_ -> Just NextPage)
                            ]
                            [Icon.mkIcon nextSvg []]
                        ]

                    ]
        ]

getProposalsPage :: State -> Array (Tuple String Status)
getProposalsPage state = (take 10 <<< drop (10 * (state.currentPage - 1))) state.proposals

chooseSvg status = case status of
    Rejected   -> fail
    Discussion -> warning
    Accepted   -> check

renderStatusBar (Tuple name status) =
    HH.li
        [HP.classes [ClassName "prop-status-item"]]
        [ HH.a
            [ HP.classes [ClassName "prop-status-link"]
            , HP.href "#"
            ]
            [ HH.span
                [HP.class_ $ ClassName "prop-status-link-text"]
                [HH.text name]
            , Icon.mkIcon (chooseSvg status) []
            ]
        ]

handleAction :: forall m. Action -> H.HalogenM State Action () Message m Unit
handleAction = case _ of
  PrevPage -> do
    state <- H.get
    let newState = if state.currentPage > 0 then state { currentPage = state.currentPage - 1 } else state
    H.put newState

  NextPage -> do
    state <- H.get
    let newState = if state.currentPage < state.lastPage then state { currentPage = state.currentPage + 1 } else state
    H.put newState

fail = """<svg height="30px" width="30px" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" id="Layer_1" x="0px" y="0px" viewBox="0 0 512 512" style="enable-background:new 0 0 512 512;" xml:space="preserve">
<ellipse style="fill:#E21B1B;" cx="256" cy="256" rx="256" ry="255.832"/>
<g>

		<rect x="228.021" y="113.143" transform="matrix(0.7071 -0.7071 0.7071 0.7071 -106.0178 256.0051)" style="fill:#FFFFFF;" width="55.991" height="285.669"/>

		<rect x="113.164" y="227.968" transform="matrix(0.7071 -0.7071 0.7071 0.7071 -106.0134 255.9885)" style="fill:#FFFFFF;" width="285.669" height="55.991"/>
</g>
<g>
</g>
<g>
</g>
<g>
</g>
<g>
</g>
<g>
</g>
<g>
</g>
<g>
</g>
<g>
</g>
<g>
</g>
<g>
</g>
<g>
</g>
<g>
</g>
<g>
</g>
<g>
</g>
<g>
</g>
</svg>"""

warning = """<svg height="30px" width="30px" xmlns="http://www.w3.org/2000/svg" id="Capa_1" enable-background="new 0 0 512 512" viewBox="0 0 512 512"><g><path d="m496.906 215.009c-61.306-71.971-127.944-138.609-199.915-199.915-23.627-20.126-58.354-20.126-81.981 0-71.972 61.306-138.61 127.944-199.916 199.915-20.126 23.627-20.126 58.354 0 81.981 61.306 71.972 127.944 138.61 199.915 199.916 23.627 20.126 58.354 20.126 81.981 0 71.972-61.306 138.61-127.944 199.916-199.915 20.125-23.628 20.125-58.354 0-81.982z" fill="#665e66"/><path d="m237.822 496.908c8.726 7.428 18.955 12.115 29.587 14.052-18.172 3.327-37.488-1.36-52.395-14.052-71.979-61.306-138.611-127.949-199.917-199.917-20.13-23.632-20.13-58.35 0-81.982 61.306-71.968 127.938-138.611 199.917-199.917 14.907-12.692 34.223-17.379 52.395-14.052-10.632 1.937-20.861 6.624-29.587 14.052-71.968 61.306-138.61 127.949-199.917 199.917-20.119 23.632-20.119 58.35 0 81.982 61.307 71.968 127.949 138.611 199.917 199.917z" fill="#544f57"/><path d="m256 473.883c-5.956 0-11.735-2.129-16.274-5.994-70.095-59.706-135.909-125.52-195.615-195.615-8.033-9.43-8.033-23.118-.001-32.548 59.706-70.095 125.521-135.909 195.615-195.615 4.539-3.866 10.318-5.995 16.274-5.995s11.735 2.129 16.274 5.994c70.095 59.706 135.909 125.52 195.615 195.615 8.033 9.43 8.033 23.118 0 32.548-59.706 70.095-125.521 135.909-195.615 195.615-4.538 3.867-10.317 5.995-16.273 5.995z" fill="#ffef4a"/><g fill="#665e66"><path d="m248.63 135.341c4.914-.357 9.827-.357 14.741 0 12.14.882 21.7 10.761 22.086 22.927 1.16 36.604 1.16 73.208 0 109.812-.385 12.166-9.946 22.045-22.086 22.927-4.914.357-9.827.357-14.741 0-12.14-.882-21.7-10.761-22.086-22.927-1.16-36.604-1.16-73.208 0-109.812.386-12.166 9.946-22.045 22.086-22.927z"/><path d="m248.981 315.305c4.679-.128 9.358-.128 14.037 0 12.689.347 22.849 10.534 23.202 23.223.14 5.027.14 10.053 0 15.08-.354 12.689-10.513 22.876-23.202 23.223-4.679.128-9.358.128-14.037 0-12.689-.347-22.849-10.534-23.202-23.223-.14-5.027-.14-10.053 0-15.08.354-12.689 10.513-22.876 23.202-23.223z"/></g></g></svg>"""

check = """<svg height="30px" width="30px" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" id="Capa_1" x="0px" y="0px" viewBox="0 0 367.805 367.805" style="enable-background:new 0 0 367.805 367.805;" xml:space="preserve">
<g>
	<path style="fill:#3BB54A;" d="M183.903,0.001c101.566,0,183.902,82.336,183.902,183.902s-82.336,183.902-183.902,183.902   S0.001,285.469,0.001,183.903l0,0C-0.288,82.625,81.579,0.29,182.856,0.001C183.205,0,183.554,0,183.903,0.001z"/>
	<polygon style="fill:#D4E1F4;" points="285.78,133.225 155.168,263.837 82.025,191.217 111.805,161.96 155.168,204.801    256.001,103.968  "/>
</g>
<g>
</g>
<g>
</g>
<g>
</g>
<g>
</g>
<g>
</g>
<g>
</g>
<g>
</g>
<g>
</g>
<g>
</g>
<g>
</g>
<g>
</g>
<g>
</g>
<g>
</g>
<g>
</g>
<g>
</g>
</svg>"""

prevSvg = """<svg width="2rem" height="2rem" xmlns="http://www.w3.org/2000/svg" aria-hidden="true" focusable="false" data-prefix="fas" data-icon="arrow-left" class="svg-inline--fa fa-arrow-left fa-w-14" role="img" viewBox="0 0 448 512"><path fill="currentColor" d="M257.5 445.1l-22.2 22.2c-9.4 9.4-24.6 9.4-33.9 0L7 273c-9.4-9.4-9.4-24.6 0-33.9L201.4 44.7c9.4-9.4 24.6-9.4 33.9 0l22.2 22.2c9.5 9.5 9.3 25-.4 34.3L136.6 216H424c13.3 0 24 10.7 24 24v32c0 13.3-10.7 24-24 24H136.6l120.5 114.8c9.8 9.3 10 24.8.4 34.3z"/></svg>"""
nextSvg = """<svg width="2rem" height="2rem" xmlns="http://www.w3.org/2000/svg" aria-hidden="true" focusable="false" data-prefix="fas" data-icon="arrow-right" class="svg-inline--fa fa-arrow-right fa-w-14" role="img" viewBox="0 0 448 512"><path fill="currentColor" d="M190.5 66.9l22.2-22.2c9.4-9.4 24.6-9.4 33.9 0L441 239c9.4 9.4 9.4 24.6 0 33.9L246.6 467.3c-9.4 9.4-24.6 9.4-33.9 0l-22.2-22.2c-9.5-9.5-9.3-25 .4-34.3L311.4 296H24c-13.3 0-24-10.7-24-24v-32c0-13.3 10.7-24 24-24h287.4L190.9 101.2c-9.8-9.3-10-24.8-.4-34.3z"/></svg>"""
