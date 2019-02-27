{-# LANGUAGE OverloadedStrings #-}

module Control.Concurrent.ActorSpec (main, spec) where

import Test.Hspec
import Control.Exception (evaluate)

import Control.Concurrent.Actor


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec


spec :: Spec
spec = do

  describe "mailbox" $ do
    it "accepts and delivers a message" $ do
      mb <- mailbox
      send mb 7 `shouldReturn` ()
      receiveMailbox mb `shouldReturn` 7
