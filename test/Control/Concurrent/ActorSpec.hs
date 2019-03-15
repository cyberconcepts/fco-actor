{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Control.Concurrent.ActorSpec (main, spec) where

import Test.Hspec
import Control.Exception (evaluate)

import BasicPrelude

import Control.Concurrent.Actor
import qualified Control.Concurrent.Actor.Logging as L


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec


spec :: Spec
spec = do

  describe "mailbox" $ do
    it "accepts and delivers a message" $ do
      mb <- mailbox
      runActor (do
          send mb 7
          receiveMessage mb) minimalContext
      `shouldReturn` 7

  describe "standard actor with 'echo' handler" $ do
    it "receives and sends back messages" $ do
      myEchoRecvBox <- mailbox
      logger <- L.spawnQueueLogger
      echo <- spawnStdActor (echoHdlr (messageBox logger)) () []
      runActor (do 
          send (messageBox echo) (EchoMsg myEchoRecvBox "My first message")
          receiveMessage myEchoRecvBox) minimalContext
        `shouldReturn` "My first message"
      myLogRecvBox <- mailbox
      runActor (do
          send (L.log_queryBox logger) (L.PopLogItem myLogRecvBox)
          receiveMessage myLogRecvBox) minimalContext
        `shouldReturn` (Just (L.Info "My first message"))
      runActor (do
          send (L.log_queryBox logger) (L.PopLogItem myLogRecvBox)
          receiveMessage myLogRecvBox) minimalContext
        `shouldReturn` Nothing


-- implementation of messages, actors, handlers, etc

data EchoMsg = EchoMsg (Mailbox Text) Text

echoHdlr :: (Mailbox L.LogData) -> MsgHandler st EchoMsg
echoHdlr loggerBox state (EchoMsg clientBox txt) = do
    send loggerBox $ L.Info txt
    send clientBox txt 
    return (Just state)
