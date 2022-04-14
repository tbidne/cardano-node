{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.Resources
  ( updateResources
  ) where

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (forM_)
import           Control.Monad.Extra (whenJust)
import qualified Data.Map.Strict as M
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
import           Data.Text (unpack)
import           Data.Word (Word64)
import           Text.Read (readMaybe)

--import Debug.Trace

import           Cardano.Tracer.Handlers.Metrics.Utils
import           Cardano.Tracer.Types

updateResources
  :: UI.Window
  -> AcceptedMetrics
  -> UI ()
updateResources _window acceptedMetrics = do
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

updateCPUTicks :: String -> Word64 -> UI ()
updateCPUTicks valueS _tns =
  whenJust (readMaybe valueS) $ \(_ticks :: Integer) -> do
    
    return () -- ns ticks tns =
--  ns { resourcesMetrics = newMetrics }
-- where
--  newMetrics =
--    currentMetrics
--      { cpuPercent = newCPUPercent
--      , cpuLast    = ticks
--      , cpuNs      = tns
--      }
--  currentMetrics = resourcesMetrics ns
--  newCPUPercent  = if cpuperc < 0
--                     then 0.0
--                     else cpuperc * 100.0
--  cpuperc        = fromIntegral (ticks - cpuLast currentMetrics) / fromIntegral 100 / tDiffInSec
--  tDiffInSec     = max 0.1 $ fromIntegral (tns - cpuNs currentMetrics) / 1000_000_000 :: Double

-- | Converts a timestamp to nanoseconds since Unix epoch.
utc2ns :: UTCTime -> Word64
utc2ns utc = fromInteger . round $ 1000_000_000 * utcTimeToPOSIXSeconds utc
