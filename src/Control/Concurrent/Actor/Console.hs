{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
--
--

module Control.Concurrent.Actor.Console (conIn, conOutHandler, demo) where

import BasicPrelude

import Control.Monad.Extra (whileM)

import Control.Concurrent.Actor (
    Behaviour (..), Channel, Listener, Message (..), MsgHandler,
    defCtlHandler, defListener, 
    newChan, send, spawn)


conIn :: Channel Text -> Listener ()
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
    inChan <- newChan
    outChan <- newChan
    spawn defListener [Behaviour outChan conOutHandler] ()
    spawn (conIn inChan) [] ()
    defListener [Behaviour inChan (demoHandler outChan)] ()
