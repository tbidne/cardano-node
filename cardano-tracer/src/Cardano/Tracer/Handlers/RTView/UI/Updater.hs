{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.Updater
  ( updateUI
  , updateMetricsUI
  ) where

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (readTVar, readTVarIO)
import           Control.Monad (forM_, unless, when)
import           Control.Monad.Extra (whenJust, whenJustM)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as M
import           Data.Set (Set, (\\))
import qualified Data.Set as S
import           Data.Text (unpack)
import           Data.Time.Calendar
import           Data.Time.Clock (UTCTime (..), addUTCTime, diffUTCTime, getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
import           Text.Read (readMaybe)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.Metrics.Utils
import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.State.TraceObjects
import           Cardano.Tracer.Handlers.RTView.UI.DataPoints
import           Cardano.Tracer.Handlers.RTView.UI.HTML.Node.Column
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Types

updateUI
  :: UI.Window
  -> ConnectedNodes
  -> DisplayedElements
  -> DataPointRequestors
  -> SavedTraceObjects
  -> PageReloadedFlag
  -> NonEmpty LoggingParams
  -> UI ()
updateUI window connectedNodes displayedElements dpRequestors savedTO reloadFlag loggingConfig = do
  (connected, displayedEls, afterReload) <- liftIO . atomically $ (,,)
    <$> readTVar connectedNodes
    <*> readTVar displayedElements
    <*> readTVar reloadFlag
  if afterReload
    then do
      -- Ok, web-page was reload (i.e. it's the first update after DOM-rendering),
      -- so displayed state should be restored immediately.
      addColumnsForConnected window connected loggingConfig
      checkNoNodesState window connected
      askNSetNodeInfo window dpRequestors connected displayedElements
      liftIO $ updateDisplayedElements displayedElements connected
      liftIO $ pageWasNotReload reloadFlag
    else do
      -- Check connected/disconnected nodes since previous UI's update.
      let displayed = S.fromList $ M.keys displayedEls
      when (connected /= displayed) $ do
        let disconnected   = displayed \\ connected -- In 'displayed' but not in 'connected'.
            newlyConnected = connected \\ displayed -- In 'connected' but not in 'displayed'.
        deleteColumnsForDisconnected window connected disconnected
        addColumnsForConnected window newlyConnected loggingConfig
        checkNoNodesState window connected
        askNSetNodeInfo window dpRequestors newlyConnected displayedElements
        liftIO $ updateDisplayedElements displayedElements connected
  -- Check if we have to update elements on the page using received 'TraceObject's.
  checkAcceptedTraceObjects window displayedElements savedTO
  setUptimeForNodes window connected displayedElements

addColumnsForConnected
  :: UI.Window
  -> Set NodeId
  -> NonEmpty LoggingParams
  -> UI ()
addColumnsForConnected window newlyConnected loggingConfig = do
  unless (S.null newlyConnected) $
    findAndShow window "main-table"
  forM_ newlyConnected $ addNodeColumn window loggingConfig

deleteColumnsForDisconnected
  :: UI.Window
  -> Set NodeId
  -> Set NodeId
  -> UI ()
deleteColumnsForDisconnected window connected disconnected = do
  forM_ disconnected $ deleteNodeColumn window
  when (S.null connected) $
    findAndHide window "main-table"

checkNoNodesState :: UI.Window -> Set NodeId -> UI ()
checkNoNodesState window connected =
  if S.null connected
    then do
      findAndShow window "no-nodes"
      findAndShow window "no-nodes-info"
    else do
      findAndHide window "no-nodes"
      findAndHide window "no-nodes-info"

checkAcceptedTraceObjects
  :: UI.Window
  -> DisplayedElements
  -> SavedTraceObjects
  -> UI ()
checkAcceptedTraceObjects window displayedElements savedTO = do
  savedTraceObjects <- liftIO $ readTVarIO savedTO
  forM_ (M.toList savedTraceObjects) $ \(nodeId, savedForNode) ->
    forM_ (M.toList savedForNode) $ \(namespace, toValue) ->
      updateElementsIfNeeded window displayedElements nodeId namespace toValue

updateElementsIfNeeded
  :: UI.Window
  -> DisplayedElements
  -> NodeId
  -> Namespace
  -> TraceObjectTValue
  -> UI ()
updateElementsIfNeeded window displayedElements nodeId namespace toValue = do
  case namespace of
    "density" -> updateElement
    "slotNum" -> return ()
    "blockNum" -> return ()
    "slotInEpoch" -> return ()
    "epoch" -> return ()
    "forks" -> return ()
    "txsInMempool"  -> return ()
    "mempoolBytes"  -> return ()
    "txsProcessedNum"  -> return ()
    "blocksForgedNum"  -> return ()
    "nodeCannotForge"  -> return ()
    "nodeIsLeaderNum"  -> return ()
    "slotsMissedNum" -> return ()
    "operationalCertificateStartKESPeriod"  -> return ()
    "operationalCertificateExpiryKESPeriod"  -> return ()
    "currentKESPeriod"  -> return ()
    "remainingKESPeriods" -> return ()
    _ -> return ()
 where
  updateElement = do
    let elId = ""
        elValue = toValue
    liftIO (getDisplayedValue displayedElements nodeId elId) >>= \case
      Nothing ->
        -- There is no displayed value for this element yet.
        setAndSave elId elValue
      Just displayedValue ->
        -- There is a value that already displayed, check if it changed.
        unless (elValue == displayedValue) $
          setAndSave elId elValue
   where
     setAndSave elId elValue = do
      findAndSet (set text $ unpack elValue) window elId
      liftIO $ saveDisplayedValue displayedElements nodeId elId elValue

updateMetricsUI
  :: UI.Window
  -> AcceptedMetrics
  -> UI ()
updateMetricsUI _window acceptedMetrics = do
  allMetrics <- liftIO $ readTVarIO acceptedMetrics
  forM_ (M.toList allMetrics) $ \(_nodeId, (ekgStore, _)) -> do
    metrics <- liftIO $ getListOfMetrics ekgStore
    forM_ metrics $ \(_metricName, _metricValue) ->
      return ()
{-
      updateMetricsElements window nodeId metricName metricValue

updateMetricsElements
  :: UI.Window
  -> NodeId
  -> MetricName
  -> MetricValue
  -> UI ()
updateMetricsElements window nodeId metricName metricValue =
  case metricName of
    "Stat.Cputicks" -> return ()
    "Mem.Resident" -> return ()
    "RTS.GcLiveBytes" -> return ()
    "RTS.GcMajorNum" -> return ()
    "RTS.GcMinorNum" -> return ()
    "RTS.Gcticks" -> return ()
    "RTS.Mutticks" -> return ()
    "Stat.Threads" -> return ()
    _ -> return ()
-}

setUptimeForNodes
  :: UI.Window
  -> Set NodeId
  -> DisplayedElements
  -> UI ()
setUptimeForNodes window connected displayedElements = do
  now <- liftIO $ getCurrentTime
  forM_ connected $ \nodeId@(NodeId anId) -> do
    let nodeStartElId  = anId <> "__node-start-time"
        nodeUptimeElId = anId <> "__node-uptime"
    whenJustM (liftIO $ getDisplayedValue displayedElements nodeId nodeStartElId) $ \tsRaw ->
      whenJust (readMaybe (unpack tsRaw) :: Maybe UTCTime) $ \startTime -> do
        let uptimeDiff = now `diffUTCTime` startTime
            uptime = uptimeDiff `addUTCTime` nullTime
            uptimeFormatted = formatTime defaultTimeLocale "%X" uptime
            daysNum = utctDay uptime `diffDays` utctDay nullTime
            uptimeWithDays = if daysNum > 0
                               -- Show days only if 'uptime' > 23:59:59.
                               then show daysNum <> "d " <> uptimeFormatted
                               else uptimeFormatted
        findAndSet (set text uptimeWithDays) window nodeUptimeElId
 where
  nullTime = UTCTime (ModifiedJulianDay 0) 0
