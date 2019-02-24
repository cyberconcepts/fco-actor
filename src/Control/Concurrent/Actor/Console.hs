{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
--
--

module Control.Concurrent.Actor.Console (conIn, conOutHandler, demo) where

import BasicPrelude

import Control.Monad.Extra (whileM)

import Control.Concurrent.Actor (
    Behaviour (..), Channel, Actor, Message (..), MsgHandler,
    defActor, defCtlHandler,
    newChan, send, spawnActor, spawnDefActor)


conIn :: Channel Text -> Actor ()
conIn parentChan _ _ =
    whileM $ getLine >>= \case
        "bye" -> send parentChan QuitMsg >> return False
        line -> send parentChan (Message line) >> return True

conOutHandler :: MsgHandler () Text
conOutHandler _ (Message line) = putStrLn line >> (return $ Just ())
conOutHandler _ msg = defCtlHandler () msg


demoHandler :: Channel Text -> MsgHandler () Text
demoHandler outChan _ (Message line) = 
    (send outChan $ Message line) >> (return $ Just ())
demoHandler outChan _ QuitMsg = (send outChan QuitMsg) >> (return Nothing)


demo :: IO ()
demo = do
    mainChan <- newChan
    outChan <- spawnDefActor conOutHandler ()
    spawnActor (conIn mainChan) [] ()
    defActor [Behaviour mainChan (demoHandler outChan)] ()
