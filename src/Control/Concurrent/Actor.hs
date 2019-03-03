{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}

-- |
-- Copyright   :  (C) 2019 team@functionalconcepts.org
-- License     :  MIT
-- Maintainer  :  Helmut Merz <helmutm@cy55.de>
-- Stability   :  experimental
-- Portability :  GHC only (requires STM)
--
-- Actors: processes communicating via mailboxes (typed channels).
--

module Control.Concurrent.Actor (
  -- * Mailbox Types
  Mailbox, StdBoxes (..), stdBoxes, ControlMsg (..),
  -- * Actors and Message Handlers
  Actor, Behaviour (..), Behavior, MsgHandler, 
  spawnActor, spawnStdActor, defActor, defCtlHandler,
  -- * Basic Messaging Functions
  mailbox, send, receive, receiveMailbox,
  -- * Utility Functions
  whileDataM
  ) where

import BasicPrelude

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM (
    STM, TChan,
    atomically, newTChan, readTChan, tryReadTChan, writeTChan)
import Control.Monad.STM (retry)


-- * Mailbox Types

-- | A 'Mailbox' is a typed channel in which messages are stored,
-- waiting to be read via a 'receive' or 'receiveMailbox' call.
type Mailbox a = TChan a

-- | A typical 'Actor' needs two 'Mailbox'es, one for receiving control messages,
-- and one for regular messages with a payload.
data StdBoxes msg = StdBoxes {
    controlBox :: Mailbox ControlMsg,
    messageBox :: Mailbox msg
}

-- | Instantiate a 'StdBoxes' object with two 'MailBox'es.
stdBoxes :: IO (StdBoxes msg)
stdBoxes = do
    ctlBox <- mailbox
    msgBox <- mailbox
    return $ StdBoxes ctlBox msgBox

-- | A special type of message for providing control information to an 'Actor'.
--
-- At the moment there is only one value, 'Quit', that tells the 'Actor'
-- to stop.
data ControlMsg = Quit


-- * Actors and Message Handlers

-- | A 'Behaviour' is a combination of a 'Mailbox' and a 'MsgHandler'.
-- The 'MsgHandler' will process a message delivered via the 'Mailbox'.
data Behaviour st = forall a. Behv (Mailbox a) (MsgHandler st a)

-- | ... in case you prefer the American spelling.
type Behavior = Behaviour

-- | A message handler is called with the current state of the 'Actor'
-- and a message. It processes the message and returns 
-- a new state value wrapped in 'Just'. 
-- Returns 'Nothing' if the actor should stop receiving.
type MsgHandler st a = st -> a -> IO (Maybe st)

-- | A 'Listener' constitutes the central receive and processing loop of an actor.
--
-- It usually consists of two or more 'Behaviour's 
-- (with corresponding 'Mailboxes's and 'MsgHandler's) and a state
-- that may be changed during processing.
--
-- Use an empty list of 'Behaviour's for 'Listeners's that do not receive
-- any 'Message's.
-- Use '()' as dummy value if there is no state.
type Listener st = [Behaviour st] -> st -> IO ()
type Actor st = Listener st

-- | Fork a new process using an 'Actor' function with the 'Behaviour's 
-- and initial state given.
spawnActor :: Actor st -> [Behaviour st] -> st -> IO ()
spawnActor actor behaviours state = 
    forkIO (actor behaviours state) >> return ()

-- | Fork a simple standard 'Actor' process with two standard 'Mailbox'es 
-- that are created during the call. 
-- Uses 'defActor' for the receive loop.
-- Returns a 'StdBoxes' object with the mailboxes created.
--
-- The first parameter is a list of mailboxes of
-- child actors that should also receive the control messages.
spawnStdActor :: [Mailbox ControlMsg] -> MsgHandler st msg -> st -> IO (StdBoxes msg)
spawnStdActor children handler state = do
    boxes <- stdBoxes
    spawnActor defActor [
        Behv (controlBox boxes) (defCtlHandler children),
        Behv (messageBox boxes) handler
      ] state
    return boxes

-- | A simple receive loop that stops when one of the handlers
-- invoked returns 'Nothing' instead of 'Just' a new state value.
defActor :: Listener st
defActor behvs = whileDataM $ \state -> receive state behvs

-- | A default handler for control messages, returning 'Nothing' when
-- called with a 'Quit'.
--
-- The first parameter is a list of mailboxes of
-- child actors that should also receive the control messages.
defCtlHandler :: [Mailbox ControlMsg] -> MsgHandler st ControlMsg
defCtlHandler children _ Quit = do
    forM children $ \box -> send box Quit
    return Nothing
--defCtlHandler state _ = return $ Just state  -- not needed yet


-- * Messaging Functions

-- | Create a new 'Mailbox'.
mailbox :: IO (Mailbox a)
mailbox = atomically newTChan

-- | Send a 'Message' to a 'Mailbox'.
send :: Mailbox a -> a -> IO ()
send mb msg = atomically $ writeTChan mb msg

-- | Check for each of the 'Behaviour's specified if there is a 
-- 'Message' waiting in the 'Mailbox' and process the first one found. 
-- If there aren't any messages it blocks until one is available.
receive :: st -> [Behaviour st] -> IO (Maybe st)
receive state behvs =
    join (atomically $ processBehv state behvs)
  where
      processBehv :: st -> [Behaviour st] -> STM (IO (Maybe st))
      processBehv _ [] = retry
      processBehv state (Behv mb handler : rest) =
        tryReadTChan mb >>= \case
            Nothing -> processBehv state rest
            Just msg -> return (handler state msg)

-- | Wait for a 'Message' in the 'Mailbox' given. This function should
-- only be used in very special cases (or for testing); 
-- in most cases you will want to use 'receive' instead.
receiveMailbox :: Mailbox a -> IO a
receiveMailbox = atomically . readTChan


-- * Utility Functions

-- | Repeat a 'Monad'ic action.
-- 
-- The action takes a state as parameter; the initial value of
-- this state is the second parameter of 'whileDataM'.
-- The action returns an updated version of the state wrapped in 'Maybe'.
-- When the action returns 'Nothing' the loop stops.
whileDataM :: Monad m => (s -> m (Maybe s)) -> s -> m ()
whileDataM act state =
    act state >>= \case
      Nothing -> return ()
      Just newState -> whileDataM act newState
