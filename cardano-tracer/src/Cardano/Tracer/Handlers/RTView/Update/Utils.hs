{-# LANGUAGE LambdaCase #-}

module Cardano.Tracer.Handlers.RTView.Update.Utils
  ( askDataPoint
  ) where

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Data.Aeson (FromJSON, decode')
import           Data.Functor ((<&>))
import qualified Data.Map.Strict as M

import           Trace.Forward.Utils.DataPoint (askForDataPoints)
import           Trace.Forward.Protocol.DataPoint.Type (DataPointName)

import           Cardano.Tracer.Types

-- | There is a different information the node can provide us by explicit request.
--   This is a structured data about internal state of the node (for example, its
--   basic information like version, protocol, commit hash, start time, etc).
--
--   Such a structured data is provided as a 'DataPoint'. When it's receved, it's
--   technically a lazy bytestring that is a result of 'ToJSON'-encoding on the
--   forwarder's side. Here we can decode it to particular Haskell type provided
--   by the node.
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
