{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main
  ( main
  ) where

import Prelude
import Spec.Shutdown qualified
import Spec.ShutdownOnSlotSynced qualified
import System.Environment qualified as E
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Ingredients qualified as T
import Test.Util qualified as H

tests :: IO TestTree
tests = pure $ T.testGroup "test/Spec.hs"
  [ T.testGroup "Spec"
    [ H.ignoreOnWindows "Shutdown" Spec.Shutdown.hprop_shutdown
    , H.ignoreOnWindows "ShutdownOnSlotSynced" Spec.ShutdownOnSlotSynced.hprop_shutdownOnSlotSynced
    ]
  ]

ingredients :: [T.Ingredient]
ingredients = T.defaultIngredients

main :: IO ()
main = do
  args <- E.getArgs

  E.withArgs args $ tests >>= T.defaultMainWithIngredients ingredients
