{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}

-- |
-- Copyright   :  (C) 2019 functionalconcepts.org team
-- License     :  MIT
-- Maintainer  :  team@functionalconcepts.org
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

import Control.Concurrent.Actor.Util (whileDataM)


-- * Types

-- | The basic 'Message' type.
data Message a = Message a  -- ^ A message with a payload of type @a@.
   | QuitMsg                -- ^ A control message, intended to stop the actor.
  deriving (Eq, Ord, Show)

-- | A 'Mailbox' is a type channel in which messages are stored,
-- waiting to be accepted via a 'receive' or 'receiveMailbox' call.
type Mailbox a = TChan (Message a)

type MsgHandler st a = st -> Message a -> IO (Maybe st)

data Behaviour st = forall a. Behaviour (Mailbox a) (MsgHandler st a)

type Actor st = [Behaviour st] -> st -> IO ()


-- * Actors and Message Handlers

-- | Fork a new process using an 'Actor' function with 'Behaviour's and a state.
spawnActor :: Actor st -> [Behaviour st] -> st -> IO ()
spawnActor actor behaviours state = 
    forkIO (actor behaviours state) >> return ()

-- | Fork a simple default 'Actor' process with just one 'Mailbox' that is 
-- created during the call. 
-- The handler for this mailbox is specified as the first argument,
-- the second being the initial state.
-- The process uses a simple receive loop (using 'defActor') that 
-- stops when the handler returns 'Nothing'.
-- Returns the mailbox of the 'Actor'.
spawnDefActor :: MsgHandler st a -> st -> IO (Mailbox a)
spawnDefActor handler state = do
    mb <- mailbox
    spawnActor defActor [Behaviour mb handler] state
    return mb

-- | A simple receive loop that stops when one of the handlers
-- invoked returns 'Nothing' instead of a new state value wrapped in 'Just'.
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

mailbox :: IO (Mailbox a)
mailbox = atomically newTChan

send :: Mailbox a -> Message a -> IO ()
send mb msg = atomically $ writeTChan mb msg

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

receiveMailbox :: Mailbox a -> IO (Message a)
receiveMailbox = atomically . readTChan

-- }}}
