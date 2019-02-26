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

module Control.Concurrent.Actor where

import BasicPrelude

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM (
    STM, TChan,
    atomically, newTChan, readTChan, tryReadTChan, writeTChan)
import Control.Monad.STM (retry)


-- * Types

-- | The basic 'Message' type.
data Message a = Message a  -- ^ A message with a payload of type @a@.
   | QuitMsg                -- ^ A control message, telling the actor to stop.
  deriving (Eq, Ord, Show)

-- | A 'Mailbox' is a type channel in which messages are stored,
-- waiting to be read via a 'receive' or 'receiveMailbox' call.
type Mailbox a = TChan (Message a)

-- | A message handler is called with the current stat of the 'Actor'
-- and a 'Message'. It processes the message and returns 
-- a new state value wrapped in 'Maybe'.
type MsgHandler st a = st -> Message a -> IO (Maybe st)

-- | A 'behaviour' is a combination of a 'Mailbox' and a 'MsgHandler'
-- that will process a message received in the 'Mailbox'.
data Behaviour st = forall a. Behaviour (Mailbox a) (MsgHandler st a)

-- | An 'Actor' consists of - usually - one or more 'Behaviour's and a state.
-- Use an empty list of 'Behaviour's for 'Actor's that do not receive
-- any 'Message's. Use '()' as dummy value if there is no state.
type Actor st = [Behaviour st] -> st -> IO ()


-- * Actors and Message Handlers

-- | Fork a new process using an 'Actor' function with the 'Behaviour's 
-- and initial state given.
spawnActor :: Actor st -> [Behaviour st] -> st -> IO ()
spawnActor actor behaviours state = 
    forkIO (actor behaviours state) >> return ()

-- | Fork a simple default 'Actor' process with just one 'Mailbox' that is 
-- created during the call, using 'defActor' for the receive loop.
-- Returns the mailbox created.
spawnDefActor :: MsgHandler st a -> st -> IO (Mailbox a)
spawnDefActor handler state = do
    mb <- mailbox
    spawnActor defActor [Behaviour mb handler] state
    return mb

-- | A simple receive loop that stops when one of the handlers
-- invoked returns 'Nothing' instead of 'Just' a new state value.
defActor :: Actor st
defActor behaviours = whileDataM $ \state -> receive state behaviours

-- | A dummy handler that does nothing when called with regular messages
-- but correctly reacts to control 'Message's.
dummyHandler :: MsgHandler st a
dummyHandler state (Message _) = return $ Just state
dummyHandler state msg = defCtlHandler state msg

-- | A default handler for control 'Message's, returning 'Nothing' when
-- called with a 'QuitMsg'.
defCtlHandler :: MsgHandler st a
defCtlHandler _ QuitMsg = return Nothing
defCtlHandler state _ = return $ Just state


-- * Messaging Functions

-- | Create a new 'Mailbox'.
mailbox :: IO (Mailbox a)
mailbox = atomically newTChan

-- | Send a 'Message' to a 'Mailbox'.
send :: Mailbox a -> Message a -> IO ()
send mb msg = atomically $ writeTChan mb msg

-- | Check for each of the 'Behaviour's specified if there is a 
-- 'Message' waiting in the 'Mailbox' and process the first one found. 
-- Blocks if there aren't any messages until one is available.
receive :: st -> [Behaviour st] -> IO (Maybe st)
receive state behaviours =
    join (atomically $ processBehv state behaviours)
  where
      processBehv :: st -> [Behaviour st] -> STM (IO (Maybe st))
      processBehv _ [] = retry
      processBehv state (Behaviour mb handler : rest) =
        tryReadTChan mb >>= \case
            Nothing -> processBehv state rest
            Just msg -> return (handler state msg)

-- | Wait for a 'Message' in the 'Mailbox' given. This function should
-- only be used in very special cases (or for testing); 
-- in most cases you will want to use 'receive' instead.
receiveMailbox :: Mailbox a -> IO (Message a)
receiveMailbox = atomically . readTChan


-- * Utility Functions

-- | Repeat an action in a monad (typically IO).
-- 
-- The action takes a state as parameter and returns an updated 
-- version of this state wrapped in a 'Maybe' value. 
-- When the action returns 'Nothing' the loop stops.
whileDataM :: Monad m => (s -> m (Maybe s)) -> s -> m ()
whileDataM act state =
    act state >>= \case
      Nothing -> return ()
      Just newState -> whileDataM act newState
