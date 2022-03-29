{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.Node.Column
  ( addNodeColumn
  , deleteNodeColumn
  -- Description (first) column.
  , hideDescriptionColumnIfNeeded
  , showDescriptionColumnIfNeeded
  ) where

import           Control.Monad (unless, void, when)
import           Control.Monad.Extra (whenJustM)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (unpack)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Types

-- | For every connected node the new column should be added.
addNodeColumn
  :: UI.Window
  -> NodeId
  -> UI ()
addNodeColumn window (NodeId anId) = do
  let id' = unpack anId
  addNodeCell "name"     [ UI.span ## (id' <> "__node-name")
                                   #. "has-text-weight-bold is-size-4"
                                   # set text "Node"
                         ]
  addNodeCell "version"  [ UI.span ## (id' <> "__node-version")
                                   # set text "—"
                         ]
  addNodeCell "protocol" [ UI.span ## (id' <> "__node-protocol")
                                   # set text "—"
                         ]
  addNodeCell "commit"   [ UI.anchor ## (id' <> "__node-commit")
                                     #. ("rt-view-href is-family-monospace has-text-weight-normal"
                                         <> " has-tooltip-multiline has-tooltip-right")
                                     # set UI.href "#"
                                     # set dataTooltip "Browse cardano-node repository on this commit"
                                     # set text "—"
                         ]
  addNodeCell "start-time"        [UI.span ## (id' <> "__node-start-time")        # set text "—"]
  addNodeCell "system-start-time" [UI.span ## (id' <> "__node-system-start-time") # set text "—"]
  addNodeCell "uptime"   [UI.span ## (id' <> "__node-uptime")   # set text "—"]
  addNodeCell "peers"    [UI.span ## (id' <> "__node-peers")    # set text "—"]
  addNodeCell "chain"    [UI.span ## (id' <> "__node-chain")    # set text "—"]
  addNodeCell "errors"   [UI.span ## (id' <> "__node-errors")   # set text "No errors"]
 where
  addNodeCell rowId cellContent =
    whenJustM (UI.getElementById window ("node-" <> rowId <> "-row")) $ \el ->
      void $ element el #+ [ UI.th #. (unpack anId <> "__column_cell rt-view-node-column-cell")
                                   #+ cellContent
                           ]
  
-- | The node was disconnected, so its column should be deleted.
deleteNodeColumn
  :: UI.Window
  -> NodeId
  -> UI ()
deleteNodeColumn window (NodeId anId) = do
  let className = anId <> "__column_cell"
  findByClassAndDo window className UI.delete

-- | If there are no connected nodes, description column should be hidden.
--   If there is at least one connected node, description column should be shown.
hideDescriptionColumnIfNeeded, showDescriptionColumnIfNeeded
  :: UI.Window -> Set NodeId -> UI ()
hideDescriptionColumnIfNeeded window connected =
  when (S.null connected) $
    findAndSet hideIt window "main-table"
showDescriptionColumnIfNeeded window connected =
  unless (S.null connected) $
    findAndSet showIt window "main-table"
