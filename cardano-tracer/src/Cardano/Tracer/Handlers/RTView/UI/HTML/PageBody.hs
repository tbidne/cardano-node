module Cardano.Tracer.Handlers.RTView.UI.HTML.PageBody
  ( mkPageBody
  ) where

import           Data.List (intersperse)
import qualified Data.List.NonEmpty as NE
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.HTML.OwnInfo (mkOwnInfo)
import           Cardano.Tracer.Handlers.RTView.UI.Utils

mkPageBody
  :: UI.Window
  -> Network
  -> UI Element
mkPageBody window networkConfig =
  UI.getBody window #+
    [ UI.div ## "preloader" #. "pageloader is-active" #+
        [ UI.span #. "title" # set text "Just a second..."
        ]
    , topNavigation
    , UI.div ## "no-nodes" #. "container is-max-widescreen has-text-centered" #+
        [ image "rt-view-no-nodes-icon" noNodesSVG
        , UI.p #. "rt-view-no-nodes-message" #+
            [ string "There are no connected nodes. Yet."
            ]
        ]
    , noNodesInfo networkConfig
    , UI.mkElement "section" #. "section" #+
        [ UI.div ## "main-table"
                 #. "table-container rt-view-main-table-container"
                 # hideIt #+
            [ UI.table #. "table rt-view-main-table" #+
                [ UI.mkElement "thead" #+
                    [ UI.tr ## "node-name-row" #+
                        [ UI.th #. "rt-view-main-table-description"
                                #+ [UI.span # set html "&nbsp;"]
                        ]
                    ]
                , UI.mkElement "tbody" #+
                    [ UI.tr ## "node-version-row" #+
                        [ UI.td #+ [ image "rt-view-overview-icon" versionSVG
                                   , string "Version"
                                   ]
                        ]
                    , UI.tr ## "node-protocol-row" #+
                        [ UI.td #+ [ image "rt-view-overview-icon" protocolSVG
                                   , string "Protocol"
                                   ]
                        ]
                    , UI.tr ## "node-commit-row" #+
                        [ UI.td #+ [ image "rt-view-overview-icon" commitSVG
                                   , string "Commit"
                                   ]
                        ]
                    , UI.tr ## "node-start-time-row" #+
                        [ UI.td #+ [ image "rt-view-overview-icon" startSVG
                                   , string "Node start"
                                   ]
                        ]
                    , UI.tr ## "node-system-start-time-row" #+
                        [ UI.td #+ [ image "rt-view-overview-icon" systemStartSVG
                                   , string "System start"
                                   ]
                        ]
                    , UI.tr ## "node-uptime-row" #+
                        [ UI.td #+ [ image "rt-view-overview-icon" uptimeSVG
                                   , string "Uptime"
                                   ]
                        ]
                    , UI.tr ## "node-logs-row" #+
                        [ UI.td #+ [ image "rt-view-overview-icon" logsSVG
                                   , string "Logs"
                                   ]
                        ]
                    , UI.tr ## "node-peers-row" #+
                        [ UI.td #+ [ image "rt-view-overview-icon" peersSVG
                                   , string "Peers"
                                   ]
                        ]
                    , UI.tr ## "node-chain-row" #+
                        [ UI.td #+ [ image "rt-view-overview-icon" chainSVG
                                   , string "Chain"
                                   ]
                        ]
                    , UI.tr ## "node-errors-row" #+
                        [ UI.td #+ [ image "rt-view-overview-icon" errorsSVG
                                   , string "Errors"
                                   ]
                        ]
                    ]
                ]
            ]
        ]
    ]

topNavigation :: UI Element
topNavigation = do
  closeInfo <- UI.button #. "modal-close is-large" #+ []
  info <- mkOwnInfo closeInfo
  infoIcon <- image "mr-4 rt-view-info-icon" rtViewInfoSVG
                    -- #. "has-tooltip-right"
                    -- # set dataTooltip "RTView info"
  registerClicksForModal info infoIcon closeInfo

  --closeNotifications <- UI.button #. "modal-close is-large" #+ []
  --notifications <- mkOwnInfo closeNotifications
  notifyIcon <- image "rt-view-notify-icon" rtViewNotifySVG
                      -- #. "has-tooltip-bottom"
                      -- # set dataTooltip "RTView notifications"
  --registerClicksForModal notifications notifyIcon closeNotifications

  UI.div #. "navbar rt-view-top-bar" #+
    [ element info
    -- , element notifications
    , UI.div #. "navbar-brand" #+
        [ UI.div #. "navbar-item" #+
            [ image "rt-view-cardano-logo" cardanoLogoSVG
            , UI.span #. "rt-view-name" # set text "Node Real-time View"
            ]
        ]
    , UI.div #. "navbar-menu" #+
        [ UI.div #. "navbar-start" #+ []
        , UI.div #. "navbar-end" #+
            [ UI.div #. "navbar-item" #+ [element notifyIcon]
            , UI.div #. "navbar-item" #+ [element infoIcon]
            ]
        ]
    ]
 where
  registerClicksForModal modal iconToOpen iconToClose = do
    on UI.click iconToOpen  . const $ element modal #. "modal is-active"
    on UI.click iconToClose . const $ element modal #. "modal"

-- | If the user doesn't see connected nodes - possible reason of it is
--   misconfiguration of 'cardano-tracer' and/or 'cardano-node'.
--   So we have to show basic explanation.
noNodesInfo :: Network -> UI Element
noNodesInfo networkConfig = do
  closeIt <- UI.button #. "delete"
  infoNote <-
    UI.div ## "no-nodes-info"
           #. "container notification is-link rt-view-no-nodes-info" #+
      [ element closeIt
      , UI.p #. "mb-4 is-size-4" # set text "«Hey, where are my nodes?»"
      , UI.p #+
          [ string "If you are sure that your nodes should already be connected, "
          , string "please check your configuration files."
          ]
      , UI.p #+
          [ string "For more details, please read "
          , UI.anchor # set UI.href "https://github.com/input-output-hk/cardano-node/blob/master/cardano-tracer/docs/cardano-tracer.md#configuration"
                      # set text "our documentation"
          , string "."
          ]
      , UI.p #. "mt-4" #+
          [ UI.span # set UI.html ("Currently, your <code>cardano-tracer</code> is configured as a " <> mode)
          , UI.span # set UI.html (", so it " <> whatItDoes)
          ]
      , UI.p #. "mt-4" #+
          [ UI.span # set UI.html ("Correspondingly, your " <> nodeConfigNotice)
          ]
      ]
  on UI.click closeIt . const $ element infoNote # hideIt
  return infoNote
 where
  (mode, whatItDoes, nodeConfigNotice) =
    case networkConfig of
      AcceptAt (LocalSocket p) ->
        ( "server"
        , "accepts connections from your nodes via the local socket <code>" <> p <> "</code>."
        , "nodes should be configured as clients: make sure <code>TraceOptionForwarder.mode</code>"
          <> " is <code>Initiator</code>, also check <code>TraceOptionForwarder.address</code> path."
        )
      ConnectTo addrs ->
        let manySocks = NE.length addrs > 1 in
        ( "client"
        , "connects to your "
          <> (if manySocks then "nodes" else "node")
          <> " via the local "
          <> (if manySocks
               then
                 let socks = map (\(LocalSocket p) -> "<code>" <> p <> "</code>") $ NE.toList addrs
                 in "sockets " <> concat (intersperse ", " socks) <> "."
               else
                 "socket <code>" <> let LocalSocket p = NE.head addrs in p <> "</code>.")
        , (if manySocks then "nodes" else "node")
          <> " should be configured as "
          <> (if manySocks then "servers" else "a server")
          <> ": make sure <code>TraceOptionForwarder.mode</code>"
          <> " is <code>Responder</code>, also check <code>TraceOptionForwarder.address</code> path."
        )
