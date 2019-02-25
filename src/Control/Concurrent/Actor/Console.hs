{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
--
--

module Control.Concurrent.Actor.Console (conIn, conOutHandler, demo) where

import BasicPrelude

import Control.Monad.Extra (whileM)

import Control.Concurrent.Actor (
    Behaviour (..), Mailbox, Actor, Message (..), MsgHandler,
    defActor, defCtlHandler,
    mailbox, send, spawnActor, spawnDefActor)


conIn :: Mailbox Text -> Actor ()
conIn parent _ _ =
    whileM $ getLine >>= \case
        "bye" -> send parent QuitMsg >> return False
        line -> send parent (Message line) >> return True

conOutHandler :: MsgHandler () Text
conOutHandler _ (Message line) = putStrLn line >> (return $ Just ())
conOutHandler _ msg = defCtlHandler () msg


demoHandler :: Mailbox Text -> MsgHandler () Text
demoHandler outbox _ (Message line) = 
    (send outbox $ Message line) >> (return $ Just ())
demoHandler outbox _ QuitMsg = (send outbox QuitMsg) >> (return Nothing)


demo :: IO ()
demo = do
    mybox <- mailbox
    outbox <- spawnDefActor conOutHandler ()
    spawnActor (conIn mybox) [] ()
    defActor [Behaviour mybox (demoHandler outbox)] ()
