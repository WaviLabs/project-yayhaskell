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

type Message = Boolean

data Action = Toggle

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
initialState _ = { isActive: false, proposals: [] }

renderStatusBar (Tuple name status) =
    HH.li
        [HP.class_ $ ClassName "prop-status-bar"]
        [HH.text $ name <> " " <> show status]

render :: forall m. State -> H.ComponentHTML Action () m
render state = case state.isActive of
    false ->
        HH.div
            [ HP.classes [ClassName "grid-item", ClassName "b"]
            , HE.onClick \_ -> Just Toggle
            ]
            [ HH.p
                []
                [HH.text "Click to Expand"]
            ]
    true  ->
        HH.div
            [ HP.classes [ClassName "grid-item", ClassName "b", ClassName "active-grid-item"]
            , HE.onClick \_ -> Just Toggle
            ]
            [ HH.ul
                [HP.class_ $ ClassName "prop-status-list"] $
                if state.proposals == [] then [HH.li [] [HH.text "No proposals"]] else map renderStatusBar state.proposals
            ]

handleAction :: forall m. Action -> H.HalogenM State Action () Message m Unit
handleAction = case _ of
  Toggle -> do
    H.modify_ (\oldState -> oldState { isActive = not oldState.isActive })
    state <- H.get
    H.raise state.isActive

-- handleQuery :: forall o m a. Query a -> H.HalogenM State Action () o m (Maybe a)
-- handleQuery = case _ of
--   GetState k -> do
--     state <- H.get
--     pure (Just (k state))
