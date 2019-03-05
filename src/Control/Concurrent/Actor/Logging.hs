{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- |
-- Copyright   :  (C) 2019 team@functionalconcepts.org
-- License     :  MIT
-- Maintainer  :  Helmut Merz <helmutm@cy55.de>
-- Stability   :  experimental
-- Portability :  GHC only (requires STM)
--
-- Actors that collect and forward log messages.
--

module Control.Concurrent.Actor.Logging where

import BasicPrelude
import qualified Data.Text as T
import Deque (Deque, fromList, cons, unsnoc)

import Control.Concurrent.Actor (
    Actor,
    Behaviour (..), ControlMsg, Mailbox, MsgHandler, StdBoxes (..),
    defContext, minimalContext,
    defCtlHandler, defListener, mailbox, send, spawnActor,
    spawnStdActor, stdBoxes)


-- * -- Definition of Log Messages

data LogData =  Debug Text
              | Info Text
              | Warning Int Text
              | Error Int Text
    deriving (Eq, Ord, Show)


-- * -- Console Output Logger

spawnConsoleLogger :: StdBoxes Text -> (Actor ()) (StdBoxes LogData)
spawnConsoleLogger output = do
    boxes <- stdBoxes
    let handler :: MsgHandler () LogData
        handler st msg = do 
          send (messageBox output) (T.pack (show msg))
          return $ Just st
    spawnStdActor minimalContext [(controlBox output)] handler ()
    return boxes


-- * -- Queryable Queue Logger

data LoggerQuery a = PopLogItem (Mailbox (Maybe a))

data LoggerBoxes a = LoggerBoxes {
    log_ctlBox    :: Mailbox ControlMsg,
    log_msgBox    :: Mailbox a,
    log_queryBox  :: Mailbox (LoggerQuery a)
}

spawnQueueLogger :: (Actor (Deque a)) (LoggerBoxes a)
spawnQueueLogger = do
    ctlBox <- mailbox
    logBox <- mailbox
    queryBox <- mailbox
    let boxes = LoggerBoxes ctlBox logBox queryBox
        logHandler qu msg = return $ Just (cons msg qu)
        queryHandler qu (PopLogItem mb) =
          case unsnoc qu of
            Nothing -> send mb Nothing >> (return $ Just qu)
            Just (msg, qu') -> send mb (Just msg) >> (return $ Just qu')
        ctx = defContext (fromList []) [
                      Behv ctlBox (defCtlHandler []),
                      Behv logBox logHandler,
                      Behv queryBox queryHandler
                    ] []
    spawnActor ctx defListener [
        Behv ctlBox (defCtlHandler []),
        Behv logBox logHandler,
        Behv queryBox queryHandler
      ] (fromList []) 
    return boxes
