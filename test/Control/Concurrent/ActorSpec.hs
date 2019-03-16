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
      runActor (do
          mb <- mailbox
          send mb 7
          receiveMessage mb) minimalContext
      `shouldReturn` 7

  describe "standard actor with 'echo' handler" $ do
    it "receives and sends back messages" $ do
      runActor (do 
          myEchoRecvBox <- mailbox
          logger <- L.spawnQueueLogger
          echo <- spawnStdActor (echoHdlr (messageBox logger)) () []
          send (messageBox echo) (EchoMsg myEchoRecvBox "My first message")
          m1 <- receiveMessage myEchoRecvBox
          myLogRecvBox <- mailbox
          send (L.log_queryBox logger) (L.PopLogItem myLogRecvBox)
          m2 <- receiveMessage myLogRecvBox
          send (L.log_queryBox logger) (L.PopLogItem myLogRecvBox)
          m3 <- receiveMessage myLogRecvBox
          return (m1, m2, m3)) minimalContext
        `shouldReturn` (
            "My first message",
            Just (L.Info "My first message"),
            Nothing)


-- implementation of messages, actors, handlers, etc

data EchoMsg = EchoMsg (Mailbox Text) Text

echoHdlr :: (Mailbox L.LogData) -> MsgHandler st EchoMsg
echoHdlr loggerBox state (EchoMsg clientBox txt) = do
    send loggerBox $ L.Info txt
    send clientBox txt 
    return (Just state)
