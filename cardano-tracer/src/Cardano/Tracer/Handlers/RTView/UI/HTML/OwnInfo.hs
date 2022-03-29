{-# LANGUAGE CPP #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.OwnInfo
  ( mkOwnInfo
  ) where

--import           Control.Monad (forM, void)
--import qualified Data.Text as T
--import           Data.Version (showVersion)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core (Element, UI, element, set, string, text,
                   (#), (#+), (#.))
--import           System.FilePath.Posix (takeDirectory)

import           Cardano.Git.Rev () -- gitRev)

--import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
--import           Cardano.Tracer.Handlers.RTView.UI.Utils
--import           Paths_cardano_tracer (version)

{-
import           Cardano.RTView.CLI (RTViewParams (..))
import           Cardano.RTView.Config (configFileIsProvided, notificationsFileIsProvided,
                                        logFilesDir, savedConfigurationFile,
                                        savedNotificationsFile, savedRTViewParamsFile)
import           Cardano.RTView.GUI.Elements (HTMLClass (..), (#.), hideIt)
import           Cardano.RTView.GUI.JS.Utils (copyTextToClipboard)
import           Cardano.RTView.Git.Rev (gitRev)
import           Paths_cardano_rt_view (version)
-}

mkOwnInfo :: Element -> UI Element
mkOwnInfo closeIt =
  UI.div #. "modal" #+
    [ UI.div #. "modal-background" #+ []
    , UI.div #. "modal-content" #+
        [ UI.div #. "container" #+
            [ UI.div #. "table-container rt-view-peers-table-container" #+
              [ UI.table #. "table rt-view-peers-table" #+
                  [ UI.mkElement "thead" #+
                      [ UI.tr #+
                          [ UI.th #+ [string "Endpoint"]
                          , UI.th #+ [string "Slots number"]
                          , UI.th #. "rt-view-narrow-th" #+
                              [UI.mkElement "abbr" # set UI.title__ "Bytes in flight" #+ [string "Bts"]]
                          , UI.th #. "rt-view-narrow-th" #+
                              [UI.mkElement "abbr" # set UI.title__ "Requests in flight" #+ [string "Req"]]
                          , UI.th #. "rt-view-narrow-th" #+
                              [UI.mkElement "abbr" # set UI.title__ "Blocks in flight" #+ [string "Blk"]]
                          , UI.th #+ [string "Status"]
                          ]
                      ]
                  , UI.mkElement "tbody" #+
                      [ UI.tr #+
                          [ UI.td #+ [UI.span #. "tag is-success is-light is-medium is-family-monospace"
                                              # set text "127.0.0.1:8100"]
                          , UI.td #+ [string "346"]
                          , UI.td #+ [string "1"]
                          , UI.td #+ [string "0"]
                          , UI.td #+ [string "0"]
                          , UI.td #+ [UI.span #. "tag is-success is-medium" # set text "Ready"]
                          ]
                      , UI.tr #+
                          [ UI.td #+ [UI.span #. "tag is-success is-light is-medium is-family-monospace"
                                              # set text "127.0.0.1:8200"]
                          , UI.td #+ [string "346"]
                          , UI.td #+ [string "1"]
                          , UI.td #+ [string "0"]
                          , UI.td #+ [string "1"]
                          , UI.td #+ [UI.span #. "tag is-success is-medium" # set text "Ready"]
                          ]
                      , UI.tr #+
                          [ UI.td #+ [UI.span #. "tag is-success is-light is-medium is-family-monospace"
                                              # set text "127.0.0.1:8300"]
                          , UI.td #+ [string "344"]
                          , UI.td #+ [string "1"]
                          , UI.td #+ [string "0"]
                          , UI.td #+ [string "0"]
                          , UI.td #+ [UI.span #. "tag is-dark is-medium" # set text "Busy"]
                          ]
                      ]
                  ]
              ]
            ]
            {-
            [ UI.div #. "box rt-view-own-info-box" #+
                [ UI.div #. "columns" #+
                    [ UI.div #. "column has-text-right" #+
                        [ UI.p #. "mb-1" #+
                            [ string "Version"
                            , whatIsItImage "Version of cardano-tracer"
                            ]
                        , UI.p #. "mb-1" #+
                            [ string "Commit"
                            , whatIsItImage "Git commit cardano-tracer was built from"
                            ]
                        , UI.p #. "mb-1" #+
                            [ string "Platform"
                            , whatIsItImage "Platform cardano-tracer is running on"
                            ]
                        , UI.p #. "mb-1" #+
                            [ string "Configuration"
                            , whatIsItImage "Path to cardano-tracer's configuration"
                            ]
                        ]
                    , UI.div #. "column has-text-weight-semibold" #+
                        [ UI.p #. "mb-1" #+ [string $ showVersion version]
                        , UI.p #. "mb-1" #+ [string . T.unpack . T.take 7 $ gitRev]
                        , UI.p #. "mb-1" #+ [string platform]
                        , UI.p #. "mb-1" #+ [string "/tmp/trali"]
                        ]
                    ]
                ]
            ]
            -}
        ]
    , element closeIt
    ]

  {-
platform :: String
#if defined(mingw32_HOST_OS)
platform = "Windows"
#elif defined(darwin_HOST_OS)
platform = "Mac"
#else
platform = "Linux"
#endif
-}
