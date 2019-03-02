{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Control.Concurrent.ActorSpec (main, spec) where

import Test.Hspec
import Control.Exception (evaluate)

import BasicPrelude

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

  describe "standard actor with 'echo' handler" $ do
    it "receives and sends back messages" $ do
      myBox <- mailbox
      logger <- stdBoxes -- spawnLogger
      echo <- spawnStdActor (echoHdlr (messageBox logger)) ()
      send (messageBox echo) (EchoMsg myBox "My first message")
      receiveMailbox myBox `shouldReturn` "My first message"


-- implementation of messages, actors, handlers, etc

data EchoMsg = EchoMsg (Mailbox Text) Text

echoHdlr :: (Mailbox Text) -> MsgHandler st (EchoMsg)
echoHdlr loggerBox state (EchoMsg clientBox txt) = do
    send loggerBox txt  -- not used yet
    send clientBox txt 
    return (Just state)
