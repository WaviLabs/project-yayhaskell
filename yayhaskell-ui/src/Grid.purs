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
import Grid.F as F
import Grid.G as G
import Grid.H as H
import Grid.I as I

import Effect.Console as Console

type Input = Unit
type Message = Void
type Action = Unit
type Query = Const Void
type State = Unit

type ChildSlots =
  ( a :: H.Slot A.Query A.Message Int
  , b :: H.Slot B.Query B.Message Int
  , c :: H.Slot C.Query C.Message Int
  , d :: H.Slot D.Query D.Message Int
  , e :: H.Slot E.Query E.Message Int
  , f :: H.Slot F.Query F.Message Int
  , g :: H.Slot G.Query G.Message Int
  , h :: H.Slot H.Query H.Message Int
  , i :: H.Slot I.Query I.Message Int
  )

_a = SProxy :: SProxy "a"
_b = SProxy :: SProxy "b"
_c = SProxy :: SProxy "c"
_d = SProxy :: SProxy "d"
_e = SProxy :: SProxy "e"
_f = SProxy :: SProxy "f"
_g = SProxy :: SProxy "g"
_h = SProxy :: SProxy "h"
_i = SProxy :: SProxy "i"

component :: forall m. MonadEffect m => H.Component HH.HTML Query Unit Void m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = unit

-- Purestack Developer
render :: forall m. State -> H.ComponentHTML Action ChildSlots m
render state =
    HH.main
        [ HP.class_ $ ClassName "grid-container" ]
        [ HH.slot _a 1 A.component unit (\msg -> Just unit)
        , HH.slot _b 2 B.component unit (\msg -> Just unit)
        , HH.slot _c 3 C.component unit (\msg -> Just unit)
        , HH.slot _d 4 D.component unit (\msg -> Just unit)
        , HH.slot _e 5 E.component unit (\msg -> Just unit)
        , HH.slot _f 6 F.component unit (\msg -> Just unit)
        , HH.slot _g 7 G.component unit (\msg -> Just unit)
        , HH.slot _h 8 H.component unit (\msg -> Just unit)
        , HH.slot _i 9 I.component unit (\msg -> Just unit)
        ]

handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action ChildSlots Message m Unit
handleAction = pure
