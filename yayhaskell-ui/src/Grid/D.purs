module Grid.D where

import Prelude

import Halogen.HTML (ClassName(..), HTML, IProp)
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
    { isActive  :: Boolean
    , proposals :: Array (Tuple String Status)
    }

type Query = Const Void

type Input   = Boolean
type Message = Unit

data Action = Toggle
            | RecieveInput Input

component :: forall f i o m. H.Component HH.HTML f Input Message m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = receive
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
            ]

initialState :: forall i. Input -> State
initialState input = { isActive: input, proposals: testProps }

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

render :: forall m. State -> H.ComponentHTML Action () m
render state = case state.isActive of
    false ->
        HH.div
            [ HP.classes [ClassName "grid-item", ClassName "d"]
            , HE.onClick \_ -> Just Toggle
            ]
            [ HH.h1
                []
                [HH.text "GHC Proposals Status"]
            , HH.ul
                [HP.classes [ClassName "prop-status-list"]] $
                map renderStatusBar state.proposals
            ]
    true  ->
        HH.div
            [ HP.classes [ClassName "grid-item", ClassName "d"]
            , HE.onClick \_ -> Just Toggle
            ]
            [ HH.h1
                []
                [HH.text "GHC Proposals Status"]
            , HH.ul
                [HP.classes [ClassName "prop-status-list"]] $
                map renderStatusBar state.proposals
            ]

handleAction :: forall m. Action -> H.HalogenM State Action () Message m Unit
handleAction = case _ of
  Toggle -> do
    H.modify_ (\oldState -> oldState { isActive = not oldState.isActive })
    H.raise unit
  RecieveInput input -> do
    H.modify_ (\oldState -> oldState { isActive = input })

receive :: Input -> Maybe Action
receive input = Just $ RecieveInput input
