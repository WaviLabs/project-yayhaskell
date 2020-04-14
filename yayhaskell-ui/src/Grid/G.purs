module Grid.G where

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

type Message = Boolean

data Action = Toggle
            -- | MessageIsActive Message

component :: forall f i o m. H.Component HH.HTML f i Message m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
    }

initialState :: forall i. i -> State
initialState _ = { isActive: false, posts: []  }

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
            [ HP.classes [ClassName "grid-item", ClassName "g"]
            , HE.onClick \_ -> Just Toggle
            -- , HE.onClick \_ -> Just $ MessageIsActive state.isActive
            ]
            [ HH.p
                []
                [HH.text "Click to Expand"]
            ]
    true  ->
        HH.div
            [ HP.classes [ClassName "grid-item", ClassName "g", ClassName "active-grid-item"]
            , HE.onClick \_ -> Just Toggle
            -- , HE.onClick \_ -> Just $ MessageIsActive state.isActive
            ]
            if state.posts == [] then [ HH.p [] [HH.text "No Posts"]] else map renderPost state.posts

handleAction :: forall m. Action -> H.HalogenM State Action () Message m Unit
handleAction = case _ of
  Toggle -> do
    H.modify_ (\oldState -> oldState { isActive = not oldState.isActive })
    state <- H.get
    H.raise state.isActive
--   MessageIsActive isActive -> H.raise isActive

-- handleQuery :: forall o m a. Query a -> H.HalogenM State Action () o m (Maybe a)
-- handleQuery = case _ of
--   GetState k -> do
--     state <- H.get
--     pure (Just (k state))
