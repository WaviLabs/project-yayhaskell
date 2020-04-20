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

render :: forall m. State -> H.ComponentHTML Action () m
render state = case state.isActive of
    false ->
        HH.div
            [ HP.classes [ClassName "grid-item"]
            , HE.onClick \_ -> Just Toggle
            ]
            [ HH.h1
                []
                [HH.text "Watch Videos"]
            , Icon.mkIcon svg
                [Icon.className "fix-icon"]
            ]
    true  ->
        HH.div
            [ HP.classes [ClassName "grid-item-center"]
            , HE.onClick \_ -> Just Toggle
            ]
            if state.posts == [] then [HH.p [] [HH.text "No Posts"]] else map renderPost state.posts

handleAction :: forall m. Action -> H.HalogenM State Action () Message m Unit
handleAction = case _ of
  Toggle -> do
    H.modify_ (\oldState -> oldState { isActive = not oldState.isActive })
    H.raise unit
  RecieveInput input -> do
    H.modify_ (\oldState -> oldState { isActive = input })

receive :: Input -> Maybe Action
receive input = Just $ RecieveInput input

svg = """<svg height="200px" width="200px" xmlns="http://www.w3.org/2000/svg" aria-hidden="true" focusable="false" data-prefix="fas" data-icon="video" class="svg-inline--fa fa-video fa-w-18" role="img" viewBox="0 0 576 512"><path fill="currentColor" d="M336.2 64H47.8C21.4 64 0 85.4 0 111.8v288.4C0 426.6 21.4 448 47.8 448h288.4c26.4 0 47.8-21.4 47.8-47.8V111.8c0-26.4-21.4-47.8-47.8-47.8zm189.4 37.7L416 177.3v157.4l109.6 75.5c21.2 14.6 50.4-.3 50.4-25.8V127.5c0-25.4-29.1-40.4-50.4-25.8z"/></svg>"""
