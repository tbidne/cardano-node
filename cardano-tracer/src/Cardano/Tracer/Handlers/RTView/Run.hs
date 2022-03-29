{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.Run
  ( runRTView
  , module Cardano.Tracer.Handlers.RTView.State.TraceObjects
  ) where

import           Control.Monad (void)
import           Control.Monad.Extra (whenJust)
import           Data.Fixed (Pico)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Time.Clock (secondsToNominalDiffTime)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core (UI, liftIO, on, set, (#), (#+))
import           System.Time.Extra (sleep)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.State.TraceObjects
import           Cardano.Tracer.Handlers.RTView.UI.CSS.Bulma
import           Cardano.Tracer.Handlers.RTView.UI.CSS.Own
import           Cardano.Tracer.Handlers.RTView.UI.HTML.PageBody
import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.Updater
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Types

-- | RTView is a part of 'cardano-tracer' that provides an ability
--   to monitor Cardano nodes in a real-time. The core idea is simple:
--   RTView periodically receives some informations from the connected
--   node(s) and displays that information on a web-page.
--
--   The web-page is built using 'threepenny-gui' library. Please note
--   Gitub-version of this library is used, not Hackage-version!
--
--   TODO ...

runRTView
  :: TracerConfig
  -> ConnectedNodes
  -> AcceptedMetrics
  -> DataPointRequestors
  -> SavedTraceObjects
  -> IO ()
runRTView TracerConfig{hasRTView, ekgRequestFreq}
          connectedNodes acceptedMetrics dpRequestors savedTO =
  whenJust hasRTView $ \(Endpoint host port) -> do
    -- Initialize displayed stuff outside of main page renderer,
    -- to be able to update corresponding elements after page reloading.
    displayedElements <- initDisplayedElements
    reloadFlag <- initPageReloadFlag
    UI.startGUI (config host port) $
      mkMainPage
        connectedNodes
        displayedElements
        acceptedMetrics
        dpRequestors
        savedTO
        reloadFlag
        ekgRequestFreq
 where
  config h p = UI.defaultConfig
    { UI.jsPort = Just . fromIntegral $ p
    , UI.jsAddr = Just . encodeUtf8 . T.pack $ h
    }

mkMainPage
  :: ConnectedNodes
  -> DisplayedElements
  -> AcceptedMetrics
  -> DataPointRequestors
  -> SavedTraceObjects
  -> PageReloadedFlag
  -> Maybe Pico
  -> UI.Window
  -> UI ()
mkMainPage connectedNodes displayedElements acceptedMetrics
           dpRequestors savedTO reloadFlag ekgFreq window = do
  void $ return window # set UI.title pageTitle
  void $ UI.getHead window #+
    [ UI.link # set UI.rel "icon"
              # set UI.href ("data:image/svg+xml;base64," <> faviconSVGBase64)
    , UI.meta # set UI.name "viewport"
              # set UI.content "width=device-width, initial-scale=1"
    , UI.mkElement "style" # set UI.html bulmaCSS
    , UI.mkElement "style" # set UI.html bulmaTooltipCSS
    , UI.mkElement "style" # set UI.html bulmaPageloaderCSS
    , UI.mkElement "style" # set UI.html ownCSS
    -- , UI.mkElement "script" # set UI.html chartJS
    ]

  pageBody <- mkPageBody window

  -- Prepare and run the timer, which will hide the page preloader.
  preloaderTimer <- UI.timer # set UI.interval 10
  on UI.tick preloaderTimer . const $ do
    liftIO $ sleep 1.0
    findAndSet (set UI.class_ "pageloader") window "preloader"
    UI.stop preloaderTimer
  UI.start preloaderTimer

  -- Prepare and run the timer, which will call 'updateUI' function every second.
  uiUpdateTimer <- UI.timer # set UI.interval 1000
  on UI.tick uiUpdateTimer . const $
    updateUI
      window
      connectedNodes
      displayedElements
      dpRequestors
      savedTO
      reloadFlag
  UI.start uiUpdateTimer

  -- The user can setup EKG request frequency (in seconds) in tracer's configuration,
  -- so we start metrics' updating in a separate timer with corresponding interval.
  let toMs dt = fromEnum dt `div` 1000000000
      ekgIntervalInMs = toMs . secondsToNominalDiffTime $ fromMaybe 1.0 ekgFreq
  uiUpdateMetricsTimer <- UI.timer # set UI.interval ekgIntervalInMs
  on UI.tick uiUpdateMetricsTimer . const $
    updateMetricsUI window acceptedMetrics
  UI.start uiUpdateMetricsTimer

  on UI.disconnect window . const $ do
    -- The connection with the browser was dropped (probably user closed the tab),
    -- so timers should be stopped.
    UI.stop uiUpdateTimer
    UI.stop uiUpdateMetricsTimer
    -- To restore current displayed state after DOM-rerendering.
    liftIO $ pageWasReload reloadFlag

  void $ UI.element pageBody
