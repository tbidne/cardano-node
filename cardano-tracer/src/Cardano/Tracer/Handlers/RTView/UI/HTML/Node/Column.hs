{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.Node.Column
  ( addNodeColumn
  , deleteNodeColumn
  ) where

import           Control.Monad (forM, void)
import           Control.Monad.Extra (whenJustM)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Text (unpack)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
import           System.FilePath ((</>))

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Types

-- | For every connected node the new column should be added.
addNodeColumn
  :: UI.Window
  -> NonEmpty LoggingParams
  -> NodeId
  -> UI ()
addNodeColumn window loggingConfig (NodeId anId) = do
  let id' = unpack anId
  ls <- logsSettings loggingConfig id'
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
  addNodeCell "start-time"        [ UI.span ## (id' <> "__node-start-time")
                                            # set text "—"
                                  ]
  addNodeCell "system-start-time" [ UI.span ## (id' <> "__node-system-start-time")
                                            # set text "—"
                                  ]
  addNodeCell "uptime"   [ UI.span ## (id' <> "__node-uptime")
                                   # set text "—"
                         ]
  addNodeCell "logs"     [ UI.span ## (id' <> "__node-logs")
                                   #+ ls -- logsSettings loggingConfig anId
                         ]
  addNodeCell "peers"    [ UI.span ## (id' <> "__node-peers")
                                   # set text "—"
                         ]
  addNodeCell "chain"    [ UI.span ## (id' <> "__node-chain")
                                   # set text "—"
                         ]
  addNodeCell "errors"   [ UI.span ## (id' <> "__node-errors")
                                   # set text "No errors"
                         ]
 where
  addNodeCell rowId cellContent =
    whenJustM (UI.getElementById window ("node-" <> rowId <> "-row")) $ \el ->
      void $ element el #+ [ UI.th #. (unpack anId <> "__column_cell rt-view-node-column-cell")
                                   #+ cellContent
                           ]

-- | The new node is already connected, so we can display its logging settings.
logsSettings
  :: NonEmpty LoggingParams
  -> String
  -> UI [UI Element]
logsSettings loggingConfig anId =
  forM (NE.toList loggingConfig) $ \(LoggingParams root mode format) ->
    case mode of
      FileMode -> do
        let pathToSubdir = root </> anId
            fmt = case format of
                    ForHuman -> "LOG"
                    ForMachine -> "JSON"
        return $ UI.p #+
                   [  string pathToSubdir
                   , UI.span #. "tag is-primary is-medium" # set text fmt
                   ]
      JournalMode ->
        return $ string "J"

-- | The node was disconnected, so its column should be deleted.
deleteNodeColumn
  :: UI.Window
  -> NodeId
  -> UI ()
deleteNodeColumn window (NodeId anId) = do
  let className = anId <> "__column_cell"
  findByClassAndDo window className UI.delete
