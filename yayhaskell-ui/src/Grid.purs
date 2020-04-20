module Grid where

import Prelude

-- Imports for lesson
import Halogen.HTML (ClassName(..), HTML, IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (ButtonType(..))
import Halogen.HTML.Properties as HP

-- Imports for scaffolding
import Data.Array ((:), replicate, zipWith)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect, class MonadEffect)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.Query as Query
import Halogen.VDom.Driver (runUI)

import Grid.A as A
import Grid.B as B
import Grid.C as C
import Grid.D as D
import Grid.E as E

import Effect.Console as Console

type Input = Unit
type Message = Void
data Action = ToggleA
            | ToggleB
            | ToggleC
            | ToggleD

type Query = Const Void
type State =
  { aIsActive :: Boolean
  , bIsActive :: Boolean
  , cIsActive :: Boolean
  , dIsActive :: Boolean
  }

type ChildSlots =
  ( a :: H.Slot A.Query A.Message Int
  , b :: H.Slot B.Query B.Message Int
  , c :: H.Slot C.Query C.Message Int
  , d :: H.Slot D.Query D.Message Int
  , e :: H.Slot E.Query E.Message Int
  )

_a = SProxy :: SProxy "a"
_b = SProxy :: SProxy "b"
_c = SProxy :: SProxy "c"
_d = SProxy :: SProxy "d"
_e = SProxy :: SProxy "e"

component :: forall m. MonadEffect m => H.Component HH.HTML Query Unit Void m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ =
    { aIsActive: true
    , bIsActive: false
    , cIsActive: false
    , dIsActive: false
    }

-- Purestack Developer
render :: forall m. State -> H.ComponentHTML Action ChildSlots m
render state =
    HH.main
        [ HP.class_ $ ClassName "grid-container"
        ]
        [ HH.slot _a 1 A.component state.aIsActive (\msg -> Just ToggleA)
        , HH.slot _b 2 B.component state.bIsActive (\msg -> Just ToggleB)
        , HH.slot _c 3 C.component state.cIsActive (\msg -> Just ToggleC)
        , HH.slot _d 4 D.component state.dIsActive (\msg -> Just ToggleD)
        , HH.slot _e 5 E.component unit (\_ -> Nothing)
        ]

handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action ChildSlots Message m Unit
handleAction = case _ of
    ToggleA -> H.modify_ (\oldState -> oldState
                                { aIsActive = true
                                , bIsActive = false
                                , cIsActive = false
                                , dIsActive = false
                                }
                         )
    ToggleB -> H.modify_ (\oldState -> oldState
                                { aIsActive = false
                                , bIsActive = true
                                , cIsActive = false
                                , dIsActive = false
                                }
                         )
    ToggleC -> H.modify_ (\oldState -> oldState
                                { aIsActive = false
                                , bIsActive = false
                                , cIsActive = true
                                , dIsActive = false
                                }
                         )
    ToggleD -> H.modify_ (\oldState -> oldState
                                { aIsActive = false
                                , bIsActive = false
                                , cIsActive = false
                                , dIsActive = true
                                }
                         )

