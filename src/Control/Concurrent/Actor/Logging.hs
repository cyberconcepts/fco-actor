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
import Deque (fromList, cons, unsnoc)

import Control.Concurrent.Actor (
    Actor,
    Behaviour (..), ControlMsg, Mailbox, Mailboxes, MsgHandler, StdBoxes,
    controlBox, messageBox,
    dummyHandler, defContext,
    defCtlHandler, mailbox, send, spawnDefActor, spawnStdActor, stdBehvs)
import Control.Concurrent.Actor.Console (spawnConOut)


-- * Definition of Log Messages

data LogData =  Debug Text
              | Info Text
              | Warning Int Text
              | Error Int Text
    deriving (Eq, Ord, Show)


-- * Null Logger

spawnNullLogger :: IO (StdBoxes LogData)
spawnNullLogger =  spawnStdActor dummyHandler () []


-- * Console Output Logger

spawnConsoleLogger :: IO (StdBoxes LogData)
spawnConsoleLogger = do
    output <- spawnConOut
    let handler :: MsgHandler () LogData
        handler st msg = do 
          send (messageBox output) (T.pack (show msg))
          return $ Just st
    spawnStdActor handler () [(controlBox output)]


-- * Queryable Queue Logger

data LoggerQuery a = PopLogItem (Mailbox (Maybe LogData))

data LoggerBoxes a = LoggerBoxes {
    log_ctlBox    :: Mailbox ControlMsg,
    log_msgBox    :: Mailbox a,
    log_queryBox  :: Mailbox (LoggerQuery LogData)
}

instance Mailboxes LoggerBoxes where
    controlBox = log_ctlBox
    messageBox = log_msgBox

spawnQueueLogger :: IO (LoggerBoxes LogData)
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
        behvs = stdBehvs boxes logHandler [Behv queryBox queryHandler]
        ctx = defContext (fromList []) behvs []
    spawnDefActor ctx
    return boxes
