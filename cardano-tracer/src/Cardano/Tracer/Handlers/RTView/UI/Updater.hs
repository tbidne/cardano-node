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
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Calendar
import           Data.Time.Clock (UTCTime (..), addUTCTime, diffUTCTime, getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
import           Text.Read (readMaybe)

import Debug.Trace

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
  -> SavedTraceObjects
  -> DataPointRequestors
  -> PageReloadedFlag
  -> NonEmpty LoggingParams
  -> UI ()
updateUI window connectedNodes displayedElements savedTO dpRequestors reloadFlag loggingConfig = do
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
  checkSavedTraceObjects window displayedElements savedTO
  setUptimeForNodes window connected displayedElements

addColumnsForConnected
  :: UI.Window
  -> Set NodeId
  -> NonEmpty LoggingParams
  -> UI ()
addColumnsForConnected window newlyConnected loggingConfig = do
  unless (S.null newlyConnected) $
    findAndShow window "main-table-container"
  forM_ newlyConnected $ addNodeColumn window loggingConfig

deleteColumnsForDisconnected
  :: UI.Window
  -> Set NodeId
  -> Set NodeId
  -> UI ()
deleteColumnsForDisconnected window connected disconnected = do
  forM_ disconnected $ deleteNodeColumn window
  when (S.null connected) $
    findAndHide window "main-table-container"

checkNoNodesState :: UI.Window -> Set NodeId -> UI ()
checkNoNodesState window connected =
  if S.null connected
    then do
      findAndShow window "no-nodes"
      findAndShow window "no-nodes-info"
    else do
      findAndHide window "no-nodes"
      findAndHide window "no-nodes-info"

checkSavedTraceObjects
  :: UI.Window
  -> DisplayedElements
  -> SavedTraceObjects
  -> UI ()
checkSavedTraceObjects window displayedElements savedTO = do
  savedTraceObjects <- liftIO $ readTVarIO savedTO
  forM_ (M.toList savedTraceObjects) $ \(nodeId, savedTOForNode) ->
    forM_ (M.toList savedTOForNode) $ \(namespace, trObValue) ->
      case namespace of
        "Cardano.Node.Peers" -> updatePeers window nodeId displayedElements trObValue
        "density" -> return () -- updateElement
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
     --where
      {-
      updateElement = do
        let elId = ""
            elValue = trObValue
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
          findAndSet (set text $ T.unpack elValue) window elId
          liftIO $ saveDisplayedValue displayedElements nodeId elId elValue
      -}

updatePeers
  :: UI.Window
  -> NodeId
  -> DisplayedElements
  -> Text
  -> UI ()
updatePeers window (NodeId anId) _displayedElements trObValue =
  if "NodeKernelPeers" `T.isInfixOf` trObValue
    then return () -- It was empty 'TraceObject' (without useful info), ignore it.
    else
      case T.words trObValue of
        [addr, status, slotNo, reqsInF, blocksInF, bytesInF] -> do
          -- If we're here, there is some new information about peers:
          -- the new peer was connected, or some data for existing peer was changed.
          liftIO . traceIO $ "__CheckPeer11: " <> T.unpack trObValue
          -- 54.241.71.219   ready      ???      0      0       0
          -- 3.23.160.18     ready    4492799    1      1     636
          findAndSet (set text $ T.unpack addr)      window $ anId <> "__peer_addr"
          findAndSet (set text $ T.unpack status)    window $ anId <> "__peer_status"
          findAndSet (set text $ T.unpack slotNo)    window $ anId <> "__peer_slotNo"
          findAndSet (set text $ T.unpack reqsInF)   window $ anId <> "__peer_reqsInF"
          findAndSet (set text $ T.unpack blocksInF) window $ anId <> "__peer_blocksInF"
          findAndSet (set text $ T.unpack bytesInF)  window $ anId <> "__peer_bytesInF"
        _ -> do
          liftIO . traceIO $ "__CheckPeer22: " <> T.unpack trObValue
          return () -- It's strange: wrong format of peers info, ignore it.

{-
ppPeer :: PeerT blk -> Text
ppPeer (PeerT cid _af status inflight) =
  Text.pack $ printf "%-15s %-8s %s" (ppCid cid) (ppStatus status) (ppInFlight inflight)

instance LogFormatting (PeerT blk) where
  forMachine _dtal (PeerT cid _af status inflight) =
    mconcat [ "peerAddress"   .= String (Text.pack . show . remoteAddress $ cid)
             , "peerStatus"    .= String (Text.pack . ppStatus $ status)
             , "peerSlotNo"    .= String (Text.pack . ppMaxSlotNo . peerFetchMaxSlotNo $ inflight)
             , "peerReqsInF"   .= String (show . peerFetchReqsInFlight $ inflight)
             , "peerBlocksInF" .= String (show . Set.size . peerFetchBlocksInFlight $ inflight)
             , "peerBytesInF"  .= String (show . peerFetchBytesInFlight $ inflight)
             ]




3.127.133.53
3.129.199.184
13.52.189.184
18.133.87.50
18.158.118.230
18.158.147.46
18.159.88.186
35.158.3.162
52.57.122.128
54.151.113.34
54.179.126.222

__CheckPeer22: 52.57.122.128   ready      ???    0      0       0, 18.159.88.186   ready      ???    0      0       0
__CheckPeer22: 52.57.122.128   ready      ???    0      0       0, 18.159.88.186   ready      ???    0      0       0
__CheckPeer22: 3.127.133.53    ready      ???    0      0       0
__CheckPeer22: 3.127.133.53    ready      ???    0      0       0
__CheckPeer22: 3.127.133.53    ready      ???    0      0       0
__CheckPeer22: 3.127.133.53    ready      ???    0      0       0
__CheckPeer22: 3.127.133.53    ready      ???    0      0       0
__CheckPeer22: 3.127.133.53    ready      ???    0      0       0
__CheckPeer22: 3.127.133.53    ready      ???    0      0       0
__CheckPeer22: 18.158.147.46   ready      ???    0      0       0, 35.158.3.162    ready      ???    0      0       0
__CheckPeer22: 18.158.147.46   ready      ???    0      0       0, 35.158.3.162    ready      ???    0      0       0
__CheckPeer11: 35.158.3.162    ready      ???    0      0       0
__CheckPeer11: 35.158.3.162    ready      ???    0      0       0
__CheckPeer22: 18.158.147.46   ready      ???    0      0       0, 35.158.3.162    ready      ???    0      0       0
__CheckPeer22: 18.158.147.46   ready      ???    0      0       0, 35.158.3.162    ready      ???    0      0       0
__CheckPeer22: 35.158.3.162    ready      ???    0      0       0
__CheckPeer22: 35.158.3.162    ready      ???    0      0       0
__CheckPeer22: 35.158.3.162    ready      ???    0      0       0, 3.13.80.218     ready      ???    0      0       0
__CheckPeer22: 35.158.3.162    ready      ???    0      0       0, 3.13.80.218     ready      ???    0      0       0
__CheckPeer11: 3.13.80.218     ready      ???    0      0       0
__CheckPeer11: 3.13.80.218     ready      ???    0      0       0
__CheckPeer22: 18.158.147.46   ready      ???    0      0       0, 35.158.3.162    ready      ???    0      0       0
__CheckPeer22: 18.158.147.46   ready      ???    0      0       0, 35.158.3.162    ready      ???    0      0       0
__CheckPeer11: 3.13.80.218     ready      ???    0      0       0
__CheckPeer11: 3.13.80.218     ready      ???    0      0       0
__CheckPeer22: 3.127.133.53    ready      ???    0      0       0
__CheckPeer22: 3.127.133.53    ready      ???    0      0       0
__CheckPeer22: 18.158.202.103  ready      ???    0      0       0, 18.184.236.47   ready      ???    0      0       0
__CheckPeer22: 18.158.202.103  ready      ???    0      0       0, 18.184.236.47   ready      ???    0      0       0
__CheckPeer11: 18.158.202.103  ready      ???    0      0       0
__CheckPeer11: 18.158.202.103  ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer22: 18.184.236.47   ready    4492799    1      1     636, 18.158.202.103  ready      ???    0      0       0
__CheckPeer22: 18.184.236.47   ready    4492799    1      1     636, 18.158.202.103  ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 18.184.236.47   ready      ???    0      0       0
__CheckPeer11: 18.184.236.47   ready      ???    0      0       0
__CheckPeer11: 18.133.59.65    ready      ???    0      0       0
__CheckPeer11: 18.133.59.65    ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 18.159.65.82    ready      ???    0      0       0
__CheckPeer11: 18.159.65.82    ready      ???    0      0       0
__CheckPeer11: 3.133.45.145    ready      ???    0      0       0
__CheckPeer11: 3.133.45.145    ready      ???    0      0       0
__CheckPeer11: 3.133.45.145    ready      ???    0      0       0
__CheckPeer11: 3.133.45.145    ready      ???    0      0       0
__CheckPeer22: 18.223.202.44   ready      ???    0      0       0, 13.57.118.58    ready      ???    0      0       0
__CheckPeer22: 18.223.202.44   ready      ???    0      0       0, 13.57.118.58    ready      ???    0      0       0
__CheckPeer11: 52.15.49.197    ready      ???    0      0       0
__CheckPeer11: 52.15.49.197    ready      ???    0      0       0
__CheckPeer22: 3.123.218.74    ready      ???    0      0       0, 52.29.138.13    ready      ???    0      0       0
__CheckPeer22: 3.123.218.74    ready      ???    0      0       0, 52.29.138.13    ready      ???    0      0       0
__CheckPeer22: 3.123.218.74    ready      ???    0      0       0, 52.29.138.13    ready      ???    0      0       0
__CheckPeer22: 3.123.218.74    ready      ???    0      0       0, 52.29.138.13    ready      ???    0      0       0
__CheckPeer22: 3.123.218.74    ready      ???    0      0       0, 52.29.138.13    ready      ???    0      0       0
__CheckPeer22: 3.123.218.74    ready      ???    0      0       0, 52.29.138.13    ready      ???    0      0       0
__CheckPeer22: 18.180.136.78   ready      ???    0      0       0, 54.179.126.222  ready      ???    0      0       0
__CheckPeer22: 18.180.136.78   ready      ???    0      0       0, 54.179.126.222  ready      ???    0      0       0
__CheckPeer22: 18.180.136.78   ready      ???    0      0       0, 54.179.126.222  ready      ???    0      0       0
__CheckPeer22: 18.180.136.78   ready      ???    0      0       0, 54.179.126.222  ready      ???    0      0       0
__CheckPeer22: 52.9.197.120    ready      ???    0      0       0, 18.180.136.78   ready      ???    0      0       0
__CheckPeer22: 52.9.197.120    ready      ???    0      0       0, 18.180.136.78   ready      ???    0      0       0
__CheckPeer22: 54.179.126.222  ready      ???    0      0       0, 18.180.136.78   ready      ???    0      0       0
__CheckPeer22: 54.179.126.222  ready      ???    0      0       0, 18.180.136.78   ready      ???    0      0       0
__CheckPeer22: 18.180.136.78   ready      ???    0      0       0, 18.136.216.144  ready      ???    0      0       0
__CheckPeer22: 18.180.136.78   ready      ???    0      0       0, 18.136.216.144  ready      ???    0      0       0
__CheckPeer22: 54.179.126.222  ready      ???    0      0       0, 18.180.136.78   ready      ???    0      0       0
__CheckPeer22: 54.179.126.222  ready      ???    0      0       0, 18.180.136.78   ready      ???    0      0       0
__CheckPeer22: 18.180.136.78   ready      ???    0      0       0, 54.179.126.222  ready      ???    0      0       0
__CheckPeer22: 18.180.136.78   ready      ???    0      0       0, 54.179.126.222  ready      ???    0      0       0
__CheckPeer11: 18.180.136.78   ready      ???    0      0       0
__CheckPeer11: 18.180.136.78   ready      ???    0      0       0
__CheckPeer11: 54.215.120.53   ready      ???    0      0       0
__CheckPeer11: 54.215.120.53   ready      ???    0      0       0













__CheckPeer11: 18.133.87.50    ready      ???    0      0       0
__CheckPeer11: 18.133.87.50    ready      ???    0      0       0
__CheckPeer11: 54.151.113.34   ready    4492799    1      1     636
__CheckPeer11: 54.151.113.34   ready    4492799    1      1     636
__CheckPeer11: 54.151.113.34   ready      ???    0      0       0
__CheckPeer11: 54.151.113.34   ready      ???    0      0       0
__CheckPeer11: 54.151.113.34   ready      ???    0      0       0
__CheckPeer11: 54.151.113.34   ready      ???    0      0       0
__CheckPeer11: 3.129.199.184   ready      ???    0      0       0
__CheckPeer11: 3.129.199.184   ready      ???    0      0       0
__CheckPeer11: 13.52.189.184   ready      ???    0      0       0
__CheckPeer11: 13.52.189.184   ready      ???    0      0       0
__CheckPeer11: 3.127.133.53    ready      ???    0      0       0
__CheckPeer11: 3.127.133.53    ready      ???    0      0       0
__CheckPeer11: 3.127.133.53    ready      ???    0      0       0
__CheckPeer11: 3.127.133.53    ready      ???    0      0       0
__CheckPeer11: 3.127.133.53    ready      ???    0      0       0
__CheckPeer11: 3.127.133.53    ready      ???    0      0       0
__CheckPeer11: 3.127.133.53    ready      ???    0      0       0
__CheckPeer11: 3.127.133.53    ready      ???    0      0       0
__CheckPeer11: 3.127.133.53    ready      ???    0      0       0
__CheckPeer11: 3.127.133.53    ready      ???    0      0       0
__CheckPeer11: 3.127.133.53    ready      ???    0      0       0
__CheckPeer11: 3.127.133.53    ready      ???    0      0       0
__CheckPeer11: 3.127.133.53    ready      ???    0      0       0
__CheckPeer11: 3.127.133.53    ready      ???    0      0       0
__CheckPeer11: 3.127.133.53    ready      ???    0      0       0
__CheckPeer11: 3.127.133.53    ready      ???    0      0       0
__CheckPeer22: 3.127.133.53    ready      ???    0      0       0, 18.158.118.230  ready      ???    0      0       0
__CheckPeer22: 3.127.133.53    ready      ???    0      0       0, 18.158.118.230  ready      ???    0      0       0
__CheckPeer22: 18.159.88.186   ready      ???    0      0       0, 52.57.122.128   ready      ???    0      0       0
__CheckPeer22: 18.159.88.186   ready      ???    0      0       0, 52.57.122.128   ready      ???    0      0       0
__CheckPeer22: 18.159.88.186   ready      ???    0      0       0, 52.57.122.128   ready      ???    0      0       0
__CheckPeer22: 18.159.88.186   ready      ???    0      0       0, 52.57.122.128   ready      ???    0      0       0
__CheckPeer22: 52.57.122.128   ready      ???    0      0       0, 18.159.88.186   ready      ???    0      0       0
__CheckPeer22: 52.57.122.128   ready      ???    0      0       0, 18.159.88.186   ready      ???    0      0       0
__CheckPeer22: 18.159.88.186   ready      ???    0      0       0, 52.57.122.128   ready      ???    0      0       0
__CheckPeer22: 18.159.88.186   ready      ???    0      0       0, 52.57.122.128   ready      ???    0      0       0
__CheckPeer22: 18.159.88.186   ready      ???    0      0       0, 52.57.122.128   ready      ???    0      0       0
__CheckPeer22: 18.159.88.186   ready      ???    0      0       0, 52.57.122.128   ready      ???    0      0       0
__CheckPeer22: 18.159.88.186   ready      ???    0      0       0, 52.57.122.128   ready      ???    0      0       0
__CheckPeer22: 18.159.88.186   ready      ???    0      0       0, 52.57.122.128   ready      ???    0      0       0
__CheckPeer11: 3.127.133.53    ready      ???    0      0       0
__CheckPeer11: 3.127.133.53    ready      ???    0      0       0
__CheckPeer22: 52.57.122.128   ready      ???    0      0       0, 3.127.133.53    ready      ???    0      0       0
__CheckPeer22: 52.57.122.128   ready      ???    0      0       0, 3.127.133.53    ready      ???    0      0       0
__CheckPeer22: 3.127.133.53    ready      ???    0      0       0, 52.57.122.128   ready      ???    0      0       0
__CheckPeer22: 3.127.133.53    ready      ???    0      0       0, 52.57.122.128   ready      ???    0      0       0
__CheckPeer22: 3.127.133.53    ready      ???    0      0       0, 52.57.122.128   ready      ???    0      0       0
__CheckPeer22: 3.127.133.53    ready      ???    0      0       0, 52.57.122.128   ready      ???    0      0       0
__CheckPeer22: 3.127.133.53    ready      ???    0      0       0, 52.57.122.128   ready      ???    0      0       0
__CheckPeer22: 3.127.133.53    ready      ???    0      0       0, 52.57.122.128   ready      ???    0      0       0
__CheckPeer22: 3.127.133.53    ready      ???    0      0       0, 52.57.122.128   ready      ???    0      0       0
__CheckPeer22: 18.159.88.186   ready      ???    0      0       0, 52.57.122.128   ready      ???    0      0       0
__CheckPeer22: 18.159.88.186   ready      ???    0      0       0, 52.57.122.128   ready      ???    0      0       0
__CheckPeer22: 18.159.88.186   ready      ???    0      0       0, 52.57.122.128   ready      ???    0      0       0
__CheckPeer22: 18.159.88.186   ready      ???    0      0       0, 52.57.122.128   ready      ???    0      0       0
__CheckPeer22: 18.159.88.186   ready      ???    0      0       0, 52.57.122.128   ready      ???    0      0       0
__CheckPeer22: 18.159.88.186   ready      ???    0      0       0, 52.57.122.128   ready      ???    0      0       0
__CheckPeer22: 18.158.147.46   ready      ???    0      0       0, 35.158.3.162    ready      ???    0      0       0
__CheckPeer22: 18.158.147.46   ready      ???    0      0       0, 35.158.3.162    ready      ???    0      0       0
__CheckPeer11: 18.158.147.46   ready      ???    0      0       0
__CheckPeer11: 18.158.147.46   ready      ???    0      0       0
__CheckPeer11: 35.158.3.162    ready      ???    0      0       0
__CheckPeer11: 35.158.3.162    ready      ???    0      0       0
__CheckPeer22: 18.158.147.46   ready      ???    0      0       0, 35.158.3.162    ready      ???    0      0       0
__CheckPeer22: 18.158.147.46   ready      ???    0      0       0, 35.158.3.162    ready      ???    0      0       0
__CheckPeer22: 35.158.3.162    ready      ???    0      0       0, 18.158.147.46   ready      ???    0      0       0
__CheckPeer22: 35.158.3.162    ready      ???    0      0       0, 18.158.147.46   ready      ???    0      0       0
__CheckPeer22: 35.158.3.162    ready      ???    0      0       0, 3.13.80.218     ready      ???    0      0       0
__CheckPeer22: 35.158.3.162    ready      ???    0      0       0, 3.13.80.218     ready      ???    0      0       0
__CheckPeer11: 3.13.80.218     ready      ???    0      0       0
__CheckPeer11: 3.13.80.218     ready      ???    0      0       0
__CheckPeer22: 18.158.147.46   ready      ???    0      0       0, 35.158.3.162    ready      ???    0      0       0
__CheckPeer22: 18.158.147.46   ready      ???    0      0       0, 35.158.3.162    ready      ???    0      0       0
__CheckPeer11: 3.13.80.218     ready      ???    0      0       0
__CheckPeer11: 3.13.80.218     ready      ???    0      0       0
__CheckPeer22: 3.127.133.53    ready      ???    0      0       0, 52.57.122.128   ready      ???    0      0       0
__CheckPeer22: 3.127.133.53    ready      ???    0      0       0, 52.57.122.128   ready      ???    0      0       0
__CheckPeer11: 18.159.88.186   ready      ???    0      0       0
__CheckPeer11: 18.159.88.186   ready      ???    0      0       0
__CheckPeer11: 18.159.88.186   ready      ???    0      0       0
__CheckPeer11: 18.159.88.186   ready      ???    0      0       0
__CheckPeer22: 18.158.202.103  ready      ???    0      0       0, 18.184.236.47   ready      ???    0      0       0
__CheckPeer22: 18.158.202.103  ready      ???    0      0       0, 18.184.236.47   ready      ???    0      0       0
__CheckPeer11: 18.158.202.103  ready      ???    0      0       0
__CheckPeer11: 18.158.202.103  ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer22: 18.184.236.47   ready    4492799    1      1     636, 18.158.202.103  ready      ???    0      0       0
__CheckPeer22: 18.184.236.47   ready    4492799    1      1     636, 18.158.202.103  ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 18.184.236.47   ready      ???    0      0       0
__CheckPeer11: 18.184.236.47   ready      ???    0      0       0
__CheckPeer11: 18.133.59.65    ready      ???    0      0       0
__CheckPeer11: 18.133.59.65    ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 52.14.186.237   ready      ???    0      0       0
__CheckPeer11: 18.159.65.82    ready      ???    0      0       0
__CheckPeer11: 18.159.65.82    ready      ???    0      0       0
__CheckPeer11: 3.133.45.145    ready      ???    0      0       0
__CheckPeer11: 3.133.45.145    ready      ???    0      0       0
__CheckPeer11: 3.133.45.145    ready      ???    0      0       0
__CheckPeer11: 3.133.45.145    ready      ???    0      0       0
__CheckPeer22: 18.223.202.44   ready      ???    0      0       0, 13.57.118.58    ready      ???    0      0       0
__CheckPeer22: 18.223.202.44   ready      ???    0      0       0, 13.57.118.58    ready      ???    0      0       0
__CheckPeer11: 52.15.49.197    ready      ???    0      0       0
__CheckPeer11: 52.15.49.197    ready      ???    0      0       0
__CheckPeer22: 3.123.218.74    ready      ???    0      0       0, 52.29.138.13    ready      ???    0      0       0
__CheckPeer22: 3.123.218.74    ready      ???    0      0       0, 52.29.138.13    ready      ???    0      0       0
__CheckPeer22: 3.123.218.74    ready      ???    0      0       0, 52.29.138.13    ready      ???    0      0       0
__CheckPeer22: 3.123.218.74    ready      ???    0      0       0, 52.29.138.13    ready      ???    0      0       0
__CheckPeer22: 3.123.218.74    ready      ???    0      0       0, 52.29.138.13    ready      ???    0      0       0
__CheckPeer22: 3.123.218.74    ready      ???    0      0       0, 52.29.138.13    ready      ???    0      0       0
__CheckPeer22: 18.180.136.78   ready      ???    0      0       0, 54.179.126.222  ready      ???    0      0       0
__CheckPeer22: 18.180.136.78   ready      ???    0      0       0, 54.179.126.222  ready      ???    0      0       0
__CheckPeer22: 18.180.136.78   ready      ???    0      0       0, 54.179.126.222  ready      ???    0      0       0
__CheckPeer22: 18.180.136.78   ready      ???    0      0       0, 54.179.126.222  ready      ???    0      0       0
__CheckPeer22: 52.9.197.120    ready      ???    0      0       0, 18.180.136.78   ready      ???    0      0       0
__CheckPeer22: 52.9.197.120    ready      ???    0      0       0, 18.180.136.78   ready      ???    0      0       0
__CheckPeer22: 54.179.126.222  ready      ???    0      0       0, 18.180.136.78   ready      ???    0      0       0
__CheckPeer22: 54.179.126.222  ready      ???    0      0       0, 18.180.136.78   ready      ???    0      0       0
__CheckPeer22: 18.180.136.78   ready      ???    0      0       0, 18.136.216.144  ready      ???    0      0       0
__CheckPeer22: 18.180.136.78   ready      ???    0      0       0, 18.136.216.144  ready      ???    0      0       0
__CheckPeer22: 54.179.126.222  ready      ???    0      0       0, 18.180.136.78   ready      ???    0      0       0
__CheckPeer22: 54.179.126.222  ready      ???    0      0       0, 18.180.136.78   ready      ???    0      0       0
__CheckPeer22: 18.180.136.78   ready      ???    0      0       0, 54.179.126.222  ready      ???    0      0       0
__CheckPeer22: 18.180.136.78   ready      ???    0      0       0, 54.179.126.222  ready      ???    0      0       0
__CheckPeer11: 18.180.136.78   ready      ???    0      0       0
__CheckPeer11: 18.180.136.78   ready      ???    0      0       0
__CheckPeer11: 54.215.120.53   ready      ???    0      0       0
__CheckPeer11: 54.215.120.53   ready      ???    0      0       0

-}
























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
      whenJust (readMaybe (T.unpack tsRaw) :: Maybe UTCTime) $ \startTime -> do
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
