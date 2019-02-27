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
  deriving (Eq, Ord, Show)

-- | A special typ of message for providing control information to an 'Actor'.
--
-- At the moment there is only one value, 'Quit', that tells the 'Actor'
-- to stop.
data ControlMsg = Quit

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


-- * Actors and Message Handlers

-- | Fork a new process using an 'Actor' function with the 'Behaviour's 
-- and initial state given.
spawnActor :: Actor st -> [Behaviour st] -> st -> IO ()
spawnActor actor behaviours state = 
    forkIO (actor behaviours state) >> return ()

-- | Fork a simple standard 'Actor' process with two standard 'Mailbox'es 
-- that are created during the call. 
-- Uses 'defActor' for the receive loop.
-- Returns a 'StdBoxes' object with the mailboxes created.
spawnStdActor :: MsgHandler st msg -> st -> IO (StdBoxes msg)
spawnStdActor handler state = do
    boxes <- stdBoxes
    spawnActor defActor [
        Behaviour (controlBox boxes) defControlHandler,
        Behaviour (messageBox boxes) handler
      ] state
    return boxes

-- | A simple receive loop that stops when one of the handlers
-- invoked returns 'Nothing' instead of 'Just' a new state value.
defActor :: Actor st
defActor behaviours = whileDataM $ \state -> receive state behaviours

-- | A default handler for control messages, returning 'Nothing' when
-- called with a 'Quit'.
defControlHandler :: MsgHandler st ControlMsg
defControlHandler _ (Message Quit) = return Nothing
defControlHandler state _ = return $ Just state


-- * Messaging Functions

-- | Create a new 'Mailbox'.
mailbox :: IO (Mailbox a)
mailbox = atomically newTChan

-- | Send a 'Message' to a 'Mailbox'.
send :: Mailbox a -> Message a -> IO ()
send mb msg = atomically $ writeTChan mb msg

-- | Check for each of the 'Behaviour's specified if there is a 
-- 'Message' waiting in the 'Mailbox' and process the first one found. 
-- If there aren't any messages it blocks until one is available.
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

-- | Repeat a 'Monad'ic action.
-- 
-- The action takes a state as parameter; the initial value of
-- this state is the second parameter of 'whileDataM'.
-- The action returns an updated version of the state wrapped in a 'Maybe' value. 
-- When the action returns 'Nothing' the loop stops.
whileDataM :: Monad m => (s -> m (Maybe s)) -> s -> m ()
whileDataM act state =
    act state >>= \case
      Nothing -> return ()
      Just newState -> whileDataM act newState
