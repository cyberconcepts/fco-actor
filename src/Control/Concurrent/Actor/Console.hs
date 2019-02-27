{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright   :  (C) 2019 team@functionalconcepts.org
-- License     :  MIT
-- Maintainer  :  Helmut Merz <helmutm@cy55.de>
-- Stability   :  experimental
-- Portability :  GHC only (requires STM)
--
-- Simple console input and output actors.
--

module Control.Concurrent.Actor.Console (conIn, conOutHandler, demo) where

import BasicPrelude

import Control.Monad.Extra (whileM)

import Control.Concurrent.Actor (
    Behaviour (..), Mailbox, Actor, Message (..), MsgHandler,
    defActor, defCtlHandler,
    mailbox, send, spawnActor, spawnDefActor)


-- | Create a console input (sysin) 'Actor'. 
--
-- Sends input lines to the 'Mailbox' given.
-- When the text "bye" is entered it sends a 'QuitMsg' message
-- to the client 'Mailbox' and stops the input loops.
conIn :: Mailbox Text -> Actor ()
conIn parent _ _ =
    whileM $ getLine >>= \case
        "bye" -> send parent QuitMsg >> return False
        line -> send parent (Message line) >> return True

-- | A message handler that writes the text received to sysout.
conOutHandler :: MsgHandler () Text
conOutHandler _ (Message line) = putStrLn line >> (return $ Just ())
conOutHandler _ msg = defCtlHandler () msg


-- | An example main function that echos text from console input 
-- (received from the 'conIn' actor) to output by sending it to
-- an actor that uses the 'conOutHandler'. 
-- Stops when "bye" is entered.
demo :: IO ()
demo = do
    mybox <- mailbox
    outbox <- spawnDefActor conOutHandler ()
    spawnActor (conIn mybox) [] ()
    defActor [Behaviour mybox (demoHandler outbox)] ()

-- internal message handler for the demo actor
demoHandler :: Mailbox Text -> MsgHandler () Text
demoHandler outbox _ (Message line) = 
    (send outbox $ Message line) >> (return $ Just ())
demoHandler outbox _ QuitMsg = (send outbox QuitMsg) >> (return Nothing)

