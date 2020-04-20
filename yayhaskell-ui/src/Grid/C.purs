module Grid.C where

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

svg = """<svg height="200px" width="200px" xmlns="http://www.w3.org/2000/svg" aria-hidden="true" focusable="false" data-prefix="fas" data-icon="code-branch" class="svg-inline--fa fa-code-branch fa-w-12" role="img" viewBox="0 0 384 512"><path fill="currentColor" d="M384 144c0-44.2-35.8-80-80-80s-80 35.8-80 80c0 36.4 24.3 67.1 57.5 76.8-.6 16.1-4.2 28.5-11 36.9-15.4 19.2-49.3 22.4-85.2 25.7-28.2 2.6-57.4 5.4-81.3 16.9v-144c32.5-10.2 56-40.5 56-76.3 0-44.2-35.8-80-80-80S0 35.8 0 80c0 35.8 23.5 66.1 56 76.3v199.3C23.5 365.9 0 396.2 0 432c0 44.2 35.8 80 80 80s80-35.8 80-80c0-34-21.2-63.1-51.2-74.6 3.1-5.2 7.8-9.8 14.9-13.4 16.2-8.2 40.4-10.4 66.1-12.8 42.2-3.9 90-8.4 118.2-43.4 14-17.4 21.1-39.8 21.6-67.9 31.6-10.8 54.4-40.7 54.4-75.9zM80 64c8.8 0 16 7.2 16 16s-7.2 16-16 16-16-7.2-16-16 7.2-16 16-16zm0 384c-8.8 0-16-7.2-16-16s7.2-16 16-16 16 7.2 16 16-7.2 16-16 16zm224-320c8.8 0 16 7.2 16 16s-7.2 16-16 16-16-7.2-16-16 7.2-16 16-16z"/></svg>"""

render :: forall m. State -> H.ComponentHTML Action () m
render state = case state.isActive of
    false ->
        HH.div
            [ HP.classes [ClassName "grid-item"]
            , HE.onClick \_ -> Just Toggle
            ]
            [ HH.h1
                []
                [HH.text "Fix Issues"]
            , Icon.mkIcon svg
                [Icon.className "fix-icon"]
            ]
    true  ->
        HH.div
            [ HP.classes [ClassName "grid-item-center"]
            , HE.onClick \_ -> Just Toggle
            ]
            if state.posts == [] then [ HH.p [] [HH.text "No Posts"]] else map renderPost state.posts

handleAction :: forall m. Action -> H.HalogenM State Action () Message m Unit
handleAction = case _ of
  Toggle -> do
    H.modify_ (\oldState -> oldState { isActive = not oldState.isActive })
    H.raise unit
  RecieveInput input -> do
    H.modify_ (\oldState -> oldState { isActive = input })

receive :: Input -> Maybe Action
receive input = Just $ RecieveInput input