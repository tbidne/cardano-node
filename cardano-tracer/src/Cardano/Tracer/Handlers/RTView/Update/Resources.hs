{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.Resources
  ( updateResources
  , updateResourcesHistory
  ) where

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (forM_)
import           Control.Monad.Extra (whenJust)
import qualified Data.Map.Strict as M
import           Data.Time.Clock (getCurrentTime)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
import           Data.Text (unpack)
import           Text.Read (readMaybe)

--import Debug.Trace

import           Cardano.Tracer.Handlers.Metrics.Utils
import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.Handlers.RTView.State.Last
import           Cardano.Tracer.Handlers.RTView.Update.Utils
import           Cardano.Tracer.Types

updateResourcesHistory
  :: AcceptedMetrics
  -> ResourcesHistory
  -> LastResources
  -> IO ()
updateResourcesHistory acceptedMetrics (ResHistory rHistory) lastResources = do
  now <- getCurrentTime
  allMetrics <- readTVarIO acceptedMetrics
  forM_ (M.toList allMetrics) $ \(nodeId, (ekgStore, _)) -> do
    metrics <- liftIO $ getListOfMetrics ekgStore
    forM_ metrics $ \(metricName, metricValue) -> do
      let valueS = unpack metricValue
      case metricName of
        "stat.cputicks" -> updateCPUTicks nodeId valueS now
        "mem.resident" -> return () -- updateMemoryBytes
        "rts.gcLiveBytes" -> return () -- updateRTSBytesUsed
        "rts.gcMajorNum" -> return () -- updateGcMajorNum
        "rts.gcMinorNum" -> return () -- updateGcMinorNum
        "rts.gcticks" -> return () -- updateGCTicks
        "rts.mutticks" -> return () -- updateMutTicks
        -- "rts.stat.threads" TODO
        _ -> return ()
 where
  updateCPUTicks nodeId valueS now =
    whenJust (readMaybe valueS) $ \(cpuTicks :: Integer) -> do
      lastOnes <- readTVarIO lastResources
      case M.lookup nodeId lastOnes of
        Nothing ->
          -- There is no last resources for this node yet.
          addNullResources lastResources nodeId
        Just resourcesForNode -> do
          let tns        = utc2ns now
              tDiffInSec = max 0.1 $ fromIntegral (tns - cpuLastNS resourcesForNode) / 1000_000_000 :: Double
              ticksDiff  = cpuTicks - cpuLastTicks resourcesForNode
              cpuV       = fromIntegral ticksDiff / fromIntegral (100 :: Integer) / tDiffInSec
              newCPUPct  = if cpuV < 0 then 0.0 else cpuV * 100.0
          addHistoricalData rHistory nodeId now "CPU" $ ValueD newCPUPct
          updateLastResources lastResources nodeId $ \current ->
            current { cpuLastTicks = cpuTicks
                    , cpuLastNS = tns
                    }

-- | Update charts.
updateResources
  :: UI.Window
  -> AcceptedMetrics
  -> UI ()
updateResources _window _acceptedMetrics = do
  return ()
  {-
  now <- utc2ns <$> liftIO getCurrentTime
  allMetrics <- liftIO $ readTVarIO acceptedMetrics
  forM_ (M.toList allMetrics) $ \(_nodeId, (ekgStore, _)) -> do
    metrics <- liftIO $ getListOfMetrics ekgStore
    forM_ metrics $ \(metricName, metricValue) -> do
      let valueS = unpack metricValue
      case metricName of
        "stat.cputicks" -> updateCPUTicks valueS now
        "mem.resident" -> return () -- updateMemoryBytes
        "rts.gcLiveBytes" -> return () -- updateRTSBytesUsed
        "rts.gcMajorNum" -> return () -- updateGcMajorNum
        "rts.gcMinorNum" -> return () -- updateGcMinorNum
        "rts.gcticks" -> return () -- updateGCTicks
        "rts.mutticks" -> return () -- updateMutTicks
        -- "rts.stat.threads" TODO
        _ -> return ()
  -}
