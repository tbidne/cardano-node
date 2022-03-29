module Cardano.Tracer.Handlers.RTView.UI.HTML.PageBody
  ( mkPageBody
  ) where

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.HTML.OwnInfo (mkOwnInfo)
import           Cardano.Tracer.Handlers.RTView.UI.Utils

mkPageBody :: UI.Window -> UI Element
mkPageBody window =
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
  infoIcon <- image "mr-4 rt-view-info-icon" rtViewInfoSVG # set UI.title__ "RTView info"
  registerClicksForModal info infoIcon closeInfo

  --closeNotifications <- UI.button #. "modal-close is-large" #+ []
  --notifications <- mkOwnInfo closeNotifications
  notifyIcon <- image "rt-view-notify-icon" rtViewNotifySVG # set UI.title__ "RTView notifications"
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
