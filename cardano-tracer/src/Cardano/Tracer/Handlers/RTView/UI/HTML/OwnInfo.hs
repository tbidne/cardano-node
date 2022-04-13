{-# LANGUAGE MultiWayIf #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.OwnInfo
  ( mkOwnInfo
  ) where

import qualified Data.Text as T
import           Data.Version (showVersion)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
import           System.Info.Extra (isWindows, isMac)

import           Cardano.Git.Rev (gitRev)

import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Paths_cardano_tracer (version)

mkOwnInfo :: Element -> UI Element
mkOwnInfo closeIt =
  UI.div #. "modal" #+
    [ UI.div #. "modal-background" #+ []
    , UI.div #. "modal-content" #+
        [ UI.div #. "container" #+
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
        ]
    , element closeIt
    ]
 where
  platform =
    if | isWindows -> "Windows"
       | isMac     -> "macOS"
       | otherwise -> "Linux"

  whatIsItImage tooltip =
    image "rt-view-what-icon has-tooltip-multiline has-tooltip-right" questionSVG
          # set dataTooltip tooltip
