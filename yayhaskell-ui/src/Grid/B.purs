module Grid.B where

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

svg = """<svg height="200px" width="200px" xmlns="http://www.w3.org/2000/svg" aria-hidden="true" focusable="false" data-prefix="fas" data-icon="brain" class="svg-inline--fa fa-brain fa-w-18" role="img" viewBox="0 0 576 512"><path fill="currentColor" d="M208 0c-29.9 0-54.7 20.5-61.8 48.2-.8 0-1.4-.2-2.2-.2-35.3 0-64 28.7-64 64 0 4.8.6 9.5 1.7 14C52.5 138 32 166.6 32 200c0 12.6 3.2 24.3 8.3 34.9C16.3 248.7 0 274.3 0 304c0 33.3 20.4 61.9 49.4 73.9-.9 4.6-1.4 9.3-1.4 14.1 0 39.8 32.2 72 72 72 4.1 0 8.1-.5 12-1.2 9.6 28.5 36.2 49.2 68 49.2 39.8 0 72-32.2 72-72V64c0-35.3-28.7-64-64-64zm368 304c0-29.7-16.3-55.3-40.3-69.1 5.2-10.6 8.3-22.3 8.3-34.9 0-33.4-20.5-62-49.7-74 1-4.5 1.7-9.2 1.7-14 0-35.3-28.7-64-64-64-.8 0-1.5.2-2.2.2C422.7 20.5 397.9 0 368 0c-35.3 0-64 28.6-64 64v376c0 39.8 32.2 72 72 72 31.8 0 58.4-20.7 68-49.2 3.9.7 7.9 1.2 12 1.2 39.8 0 72-32.2 72-72 0-4.8-.5-9.5-1.4-14.1 29-12 49.4-40.6 49.4-73.9z"/></svg>"""

-- TODO: Make slight changes on hover to expose some info
render :: forall m. State -> H.ComponentHTML Action () m
render state = case state.isActive of
    false ->
        HH.div
            [ HP.classes [ClassName "grid-item", ClassName "b"]
            , HE.onClick \_ -> Just Toggle
            ]
            [ HH.h1
                []
                [HH.text "Learn Haskell"]
            , Icon.mkIcon svg
                [Icon.className "brain-icon"]
            ]
    true  ->
        HH.div
            [ HP.classes [ClassName "grid-center"]
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
