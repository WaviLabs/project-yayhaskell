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
-- import Grid.E as E
-- import Grid.F as F
-- import Grid.G as G
-- import Grid.H as H
-- import Grid.I as I
-- import Grid.J as J
-- import Grid.K as K
-- import Grid.L as L
import Grid.Free as Free

import Effect.Console as Console

type Input = Unit
type Message = Void
data Action = ToggleA
            | ToggleB
            | ToggleC
            | ToggleD
            -- | ToggleE
            -- | ToggleF
            -- | ToggleG
            -- | ToggleH
            -- | ToggleI
            -- | ToggleJ
            -- | ToggleK
            -- | ToggleL

type Query = Const Void
type State =
  { aIsActive :: Boolean
  , bIsActive :: Boolean
  , cIsActive :: Boolean
  , dIsActive :: Boolean
  -- , eIsActive :: Boolean
  -- , fIsActive :: Boolean
  -- , gIsActive :: Boolean
  -- , hIsActive :: Boolean
  -- , iIsActive :: Boolean
  -- , jIsActive :: Boolean
  -- , kIsActive :: Boolean
  -- , lIsActive :: Boolean
  }

type ChildSlots =
  ( a :: H.Slot A.Query A.Message Int
  , b :: H.Slot B.Query B.Message Int
  , c :: H.Slot C.Query C.Message Int
  , d :: H.Slot D.Query D.Message Int
  -- , e :: H.Slot E.Query E.Message Int
  -- , f :: H.Slot F.Query F.Message Int
  -- , g :: H.Slot G.Query G.Message Int
  -- , h :: H.Slot H.Query H.Message Int
  -- , i :: H.Slot I.Query I.Message Int
  -- , j :: H.Slot J.Query J.Message Int
  -- , k :: H.Slot K.Query K.Message Int
  -- , l :: H.Slot L.Query L.Message Int
  , free :: H.Slot Free.Query Free.Message Int
  )

_a = SProxy :: SProxy "a"
_b = SProxy :: SProxy "b"
_c = SProxy :: SProxy "c"
_d = SProxy :: SProxy "d"
-- _e = SProxy :: SProxy "e"
-- _f = SProxy :: SProxy "f"
-- _g = SProxy :: SProxy "g"
-- _h = SProxy :: SProxy "h"
-- _i = SProxy :: SProxy "i"
-- _j = SProxy :: SProxy "j"
-- _k = SProxy :: SProxy "k"
-- _l = SProxy :: SProxy "l"
_free = SProxy :: SProxy "free"

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
    -- , eIsActive: false
    -- , fIsActive: false
    -- , gIsActive: false
    -- , hIsActive: false
    -- , iIsActive: false
    -- , jIsActive: false
    -- , kIsActive: false
    -- , lIsActive: false
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
        , HH.slot _d 4 D.component state.dIsActive (\msg -> Nothing)
        -- , HH.slot _e 5 E.component state.eIsActive (\msg -> Just ToggleE)
        -- , HH.slot _f 6 F.component state.fIsActive (\msg -> Just ToggleF)
        -- , HH.slot _g 7 G.component state.gIsActive (\msg -> Just ToggleG)
        -- , HH.slot _h 8 H.component state.hIsActive (\msg -> Just ToggleH)
        -- , HH.slot _i 9 I.component state.iIsActive (\msg -> Just ToggleI)
        -- , HH.slot _j 10 J.component state.jIsActive (\msg -> Just ToggleJ)
        -- , HH.slot _k 11 K.component state.kIsActive (\msg -> Just ToggleK)
        -- , HH.slot _l 12 L.component state.lIsActive (\msg -> Just ToggleL)
        , HH.slot _free 13 (Free.component Free.render) unit absurd
        ]

handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action ChildSlots Message m Unit
handleAction = case _ of
    ToggleA -> H.modify_ (\oldState -> oldState
                                { aIsActive = true
                                , bIsActive = false
                                , cIsActive = false
                                , dIsActive = false
                                -- , eIsActive = false
                                -- , fIsActive = false
                                -- , gIsActive = false
                                -- , hIsActive = false
                                -- , iIsActive = false
                                -- , jIsActive = false
                                -- , kIsActive = false
                                -- , lIsActive = false
                                }
                         )
    ToggleB -> H.modify_ (\oldState -> oldState
                                { aIsActive = false
                                , bIsActive = true
                                , cIsActive = false
                                , dIsActive = false
                                -- , eIsActive = false
                                -- , fIsActive = false
                                -- , gIsActive = false
                                -- , hIsActive = false
                                -- , iIsActive = false
                                -- , jIsActive = false
                                -- , kIsActive = false
                                -- , lIsActive = false
                                }
                         )
    ToggleC -> H.modify_ (\oldState -> oldState
                                { aIsActive = false
                                , bIsActive = false
                                , cIsActive = true
                                , dIsActive = false
                                -- , eIsActive = false
                                -- , fIsActive = false
                                -- , gIsActive = false
                                -- , hIsActive = false
                                -- , iIsActive = false
                                -- , jIsActive = false
                                -- , kIsActive = false
                                -- , lIsActive = false
                                }
                         )
    ToggleD -> H.modify_ (\oldState -> oldState
                                { aIsActive = false
                                , bIsActive = false
                                , cIsActive = false
                                , dIsActive = true
                                -- , eIsActive = false
                                -- , fIsActive = false
                                -- , gIsActive = false
                                -- , hIsActive = false
                                -- , iIsActive = false
                                -- , jIsActive = false
                                -- , kIsActive = false
                                -- , lIsActive = false
                                }
                         )
{-
    ToggleE -> H.modify_ (\oldState -> oldState
                                { aIsActive = false
                                , bIsActive = false
                                , cIsActive = false
                                , dIsActive = false
                                , eIsActive = true
                                , fIsActive = false
                                , gIsActive = false
                                , hIsActive = false
                                -- , iIsActive = false
                                -- , jIsActive = false
                                -- , kIsActive = false
                                -- , lIsActive = false
                                }
                         )
    ToggleF -> H.modify_ (\oldState -> oldState
                                { aIsActive = false
                                , bIsActive = false
                                , cIsActive = false
                                , dIsActive = false
                                , eIsActive = false
                                , fIsActive = true
                                , gIsActive = false
                                , hIsActive = false
                                -- , iIsActive = false
                                -- , jIsActive = false
                                -- , kIsActive = false
                                -- , lIsActive = false
                                }
                         )
    ToggleG -> H.modify_ (\oldState -> oldState
                                { aIsActive = false
                                , bIsActive = false
                                , cIsActive = false
                                , dIsActive = false
                                , eIsActive = false
                                , fIsActive = false
                                , gIsActive = true
                                , hIsActive = false
                                -- , iIsActive = false
                                -- , jIsActive = false
                                -- , kIsActive = false
                                -- , lIsActive = false
                                }
                         )
    ToggleH -> H.modify_ (\oldState -> oldState
                                { aIsActive = false
                                , bIsActive = false
                                , cIsActive = false
                                , dIsActive = false
                                , eIsActive = false
                                , fIsActive = false
                                , gIsActive = false
                                , hIsActive = true
                                -- , iIsActive = false
                                -- , jIsActive = false
                                -- , kIsActive = false
                                -- , lIsActive = false
                                }
                         )
-}
{-
    ToggleI -> H.modify_ (\oldState -> oldState
                                { aIsActive = false
                                , bIsActive = false
                                , cIsActive = false
                                , dIsActive = false
                                , eIsActive = false
                                , fIsActive = false
                                , gIsActive = false
                                , hIsActive = false
                                , iIsActive = true
                                , jIsActive = false
                                , kIsActive = false
                                , lIsActive = false
                                }
                         )
    ToggleJ -> H.modify_ (\oldState -> oldState
                                { aIsActive = false
                                , bIsActive = false
                                , cIsActive = false
                                , dIsActive = false
                                , eIsActive = false
                                , fIsActive = false
                                , gIsActive = false
                                , hIsActive = false
                                , iIsActive = false
                                , jIsActive = true
                                , kIsActive = false
                                , lIsActive = false
                                }
                         )
    ToggleK -> H.modify_ (\oldState -> oldState
                                { aIsActive = false
                                , bIsActive = false
                                , cIsActive = false
                                , dIsActive = false
                                , eIsActive = false
                                , fIsActive = false
                                , gIsActive = false
                                , hIsActive = false
                                , iIsActive = false
                                , jIsActive = false
                                , kIsActive = true
                                , lIsActive = false
                                }
                         )
    ToggleL -> H.modify_ (\oldState -> oldState
                                { aIsActive = false
                                , bIsActive = false
                                , cIsActive = false
                                , dIsActive = false
                                , eIsActive = false
                                , fIsActive = false
                                , gIsActive = false
                                , hIsActive = false
                                , iIsActive = false
                                , jIsActive = false
                                , kIsActive = false
                                , lIsActive = true
                                }
                         )
-}
