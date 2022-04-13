{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.DataPoints
  ( askNSetNodeInfo
  ) where

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (forM_, unless)
import           Control.Monad.Extra (whenJustM)
import           Data.Aeson (FromJSON, decode')
import           Data.Functor ((<&>))
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Trace.Forward.Utils.DataPoint (askForDataPoints)
import           Trace.Forward.Protocol.DataPoint.Type (DataPointName)

import           Cardano.Node.Startup (NodeInfo (..))

import           Cardano.Tracer.Handlers.RTView.State.Displayed
-- import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Types

-- | There is a different information the node can provide us by explicit request.
--   This is a structured data about internal state of the node (for example, its
--   basic information like version, protocol, commit hash, start time, etc).
--
--   Such a structured data is provided as a 'DataPoint'. When it's receved, it's
--   technically a lazy bytestring that is a result of 'ToJSON'-encoding on the
--   forwarder's side. Here we can decode it to particular Haskell type provided
--   by the node.
--
--   The common algorithm is this:
--
--   1. Ask for 'DataPoint' using its name and decode it to Haskell type.
--   2. Set the values of corresponding elements on the web page.
--   3. Save these values in 'displayedElements' to keep them for future.
--      It helps to avoid asking this data again after DOM re-rendering
--      (for example, after the user reloaded the web page).

askNSetNodeInfo
  :: UI.Window
  -> DataPointRequestors
  -> Set NodeId
  -> DisplayedElements
  -> UI ()
askNSetNodeInfo window dpRequestors newlyConnected displayedElements =
  unless (S.null newlyConnected) $
    forM_ newlyConnected $ \nodeId@(NodeId anId) ->
      whenJustM (liftIO $ askDataPoint dpRequestors nodeId "NodeInfo") $ \ni -> do
        findAndSet' (niName ni) (anId <> "__node-name")
        findAndSet' (niVersion ni) (anId <> "__node-version")
        setProtocol (niProtocol ni) (anId <> "__node-protocol")
        findAndSet' (T.take 7 $ niCommit ni) (anId <> "__node-commit")
        findAndSet  (set UI.href $ nodeLink (niCommit ni)) window (anId <> "__node-commit")
        let nodeStartElId = anId <> "__node-start-time"
        setTime (niStartTime ni) nodeStartElId
        setTime (niSystemStartTime ni) (anId <> "__node-system-start-time")
        liftIO $ saveDisplayedValue
                   displayedElements
                   nodeId
                   nodeStartElId
                   (T.pack . show $ niStartTime ni)
 where
  findAndSet' t = findAndSet (set text $ T.unpack t) window

  nodeLink commit = T.unpack $ "https://github.com/input-output-hk/cardano-node/commit/" <> T.take 7 commit

  setProtocol p id' = do
    findAndSet' "" id'
    let byronTag   = UI.span #. "tag is-warning is-rounded is-medium" # set text "Byron"
        shelleyTag = UI.span #. "tag is-info is-rounded is-medium ml-3" # set text "Shelley"
    case p of
      "Byron"   -> findAndAdd [byronTag] window id'
      "Shelley" -> findAndAdd [shelleyTag] window id'
      _         -> findAndAdd [byronTag, shelleyTag] window id'

  setTime ts id' = do
    findAndSet' "" id'
    let time = formatTime defaultTimeLocale "%b %e, %Y %T" ts
        tz   = formatTime defaultTimeLocale "%Z" ts
    findAndAdd [ string time
               , string " "
               , UI.span #. "has-text-weight-normal is-size-6" # set text tz
               ] window id'

-- | Each 'DataPoint' contains lazy bytestring that can be decoded
--   to the value of particular type.
askDataPoint
  :: FromJSON a
  => DataPointRequestors
  -> NodeId
  -> DataPointName
  -> IO (Maybe a)
askDataPoint dpRequestors nodeId dpName =
  readTVarIO dpRequestors <&> M.lookup nodeId >>= \case
    Nothing -> return Nothing
    Just dpRequestor ->
      askForDataPoints dpRequestor [dpName] <&> lookup dpName >>= \case
        Just (Just rawValue) -> return $ decode' rawValue
        _ -> return Nothing
