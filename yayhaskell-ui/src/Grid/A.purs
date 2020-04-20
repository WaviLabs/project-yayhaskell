module Grid.A where

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

type Post =
    { author      :: String
    , link        :: String
    , commentLink :: String
    , lambdas     :: Int
    }

type State =
    { isActive :: Boolean
    , posts    :: Array Post
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

initialState :: forall i. Input -> State
initialState input = { isActive: input, posts: [] }

renderPost post =
    HH.div
        [ HP.class_ $ ClassName "news-item" ]
        [ HH.a
            [HP.class_ $ ClassName "post-title", HP.href post.link]
            [HH.text post.author]
        , HH.a
            [HP.class_ $ ClassName "post-comment-link", HP.href post.commentLink]
            [HH.text "comments"]
        , HH.p
            [HP.class_ $ ClassName "post-lambdas"]
            [HH.text $ "Lambdas: " <> show post.lambdas]
        ]

svg = """<svg height="200px" width="200px" xmlns="http://www.w3.org/2000/svg" aria-hidden="true" focusable="false" data-prefix="far" data-icon="newspaper" class="svg-inline--fa fa-newspaper fa-w-18" role="img" viewBox="0 0 576 512"><path fill="currentColor" d="M552 64H112c-20.858 0-38.643 13.377-45.248 32H24c-13.255 0-24 10.745-24 24v272c0 30.928 25.072 56 56 56h496c13.255 0 24-10.745 24-24V88c0-13.255-10.745-24-24-24zM48 392V144h16v248c0 4.411-3.589 8-8 8s-8-3.589-8-8zm480 8H111.422c.374-2.614.578-5.283.578-8V112h416v288zM172 280h136c6.627 0 12-5.373 12-12v-96c0-6.627-5.373-12-12-12H172c-6.627 0-12 5.373-12 12v96c0 6.627 5.373 12 12 12zm28-80h80v40h-80v-40zm-40 140v-24c0-6.627 5.373-12 12-12h136c6.627 0 12 5.373 12 12v24c0 6.627-5.373 12-12 12H172c-6.627 0-12-5.373-12-12zm192 0v-24c0-6.627 5.373-12 12-12h104c6.627 0 12 5.373 12 12v24c0 6.627-5.373 12-12 12H364c-6.627 0-12-5.373-12-12zm0-144v-24c0-6.627 5.373-12 12-12h104c6.627 0 12 5.373 12 12v24c0 6.627-5.373 12-12 12H364c-6.627 0-12-5.373-12-12zm0 72v-24c0-6.627 5.373-12 12-12h104c6.627 0 12 5.373 12 12v24c0 6.627-5.373 12-12 12H364c-6.627 0-12-5.373-12-12z"/></svg>"""

-- TODO: Make slight changes on hover to expose some info
render :: forall m. State -> H.ComponentHTML Action () m
render state = case state.isActive of
    false ->
        HH.div
            [ HP.classes [ClassName "grid-item"]
            , HE.onClick \_ -> Just Toggle
            ]
            [ HH.h1
                []
                [HH.text "Read News"]
            , Icon.mkIcon svg
                [Icon.className "news-icon"]
            ]
    true  ->
        HH.div
            [ HP.classes [ClassName "grid-item-center"]
            , HE.onClick \_ -> Just Toggle
            ]
            if state.posts == [] then [HH.h1 [] [HH.text "No Posts"]] else map renderPost state.posts

handleAction :: forall m. Action -> H.HalogenM State Action () Message m Unit
handleAction = case _ of
  Toggle -> do
    H.modify_ (\oldState -> oldState { isActive = not oldState.isActive })
    H.raise unit
  RecieveInput input -> do
    H.modify_ (\oldState -> oldState { isActive = input })

receive :: Input -> Maybe Action
receive input = Just $ RecieveInput input
