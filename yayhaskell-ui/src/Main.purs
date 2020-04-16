module Main where

import Prelude

import Control.Coroutine as CR
import Data.Maybe (maybe, Maybe(..))
import Effect.Class (liftEffect)
import Effect (Effect)
import Effect.Aff (Aff, error, throwError)
import Halogen.Aff (awaitBody, selectElement, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Navbar as Navbar
import Grid as Grid
import Script as Script
import Web.DOM.ParentNode (QuerySelector)
import Web.HTML (HTMLElement)
import ToggleViewMode (toggleViewMode)

-- | Here we wait for the page to load. Then we find the target element
-- | and run the component as a child of that element.
main :: Effect Unit
main =
    runHalogenAff do
      body <- awaitBody
      io   <- runUI Navbar.component unit body
      _    <- runUI Grid.component unit body
      _    <- runUI (Script.component Script.html) unit body
      io.subscribe $ CR.consumer \_ -> do
        liftEffect toggleViewMode
        pure $ Nothing
      pure unit
  where
    selectElement' :: String -> QuerySelector -> Aff HTMLElement
    selectElement' errorMessage query = do
      maybeElem <- selectElement query
      maybe (throwError (error errorMessage)) pure maybeElem
