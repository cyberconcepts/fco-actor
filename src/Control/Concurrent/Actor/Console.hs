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

module Control.Concurrent.Actor.Console (conInActor, conOutHandler, demo) where

import BasicPrelude

import Control.Monad.Extra (whileM)

import Control.Concurrent.Actor ( 
    ControlMsg (..), Listener, MsgHandler, StdBoxes (..),
    defListener, forward, minimalContext, runActor, send, 
    spawnActor, stdContext)


-- | Create a console input (stdin) 'Listener'. 
--
-- Sends input lines to the parent 'messageBox'.
-- When the text "bye" is entered it sends a 'Quit' message
-- to the parent 'controlBox' and stops the input loop.
conInActor :: (StdBoxes Text) -> Listener ()
conInActor parent =
    whileM $ getLine >>= \case
        "bye" -> send (controlBox parent) Quit >> return False
        line -> send (messageBox parent) line >> return True

-- | A message handler that writes the text received to stdout.
conOutHandler :: MsgHandler () Text
conOutHandler _ line = putStrLn line >> return (Just ())

-- | An example main function that echos text from console input 
-- (received via the 'conInLoop' actor) to output by sending it to
-- a console output actor that uses 'conOutHandler'.
-- Stops when "bye" is entered.
demo :: IO ()
demo = do
    (output, outCtx) <- stdContext conOutHandler () []
    (self, selfCtx) <- stdContext 
                            (forward [messageBox output]) ()
                            [controlBox output]
    spawnActor outCtx defListener
    spawnActor minimalContext (conInActor self)
    runActor defListener selfCtx
