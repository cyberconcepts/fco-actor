{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
  -- * Actors and Actor Contexts
  Actor, Context (..), 
  defContext, minimalContext, stdContext, runActor,
  -- * Listeners and Message Handlers
  Behaviour (..), Behavior, Listener, MsgHandler, 
  forward,
  spawnActor, spawnStdActor, defCtlHandler, defListener, stdListener,
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
import Control.Monad.Trans.State (StateT, evalStateT)


-- * Mailbox Types

-- | A 'Mailbox' is a typed channel in which messages are stored,
-- waiting to be read via a 'receive' or 'receiveMailbox' call.
type Mailbox a = TChan a

-- | A typical 'Listener' needs two 'Mailbox'es, one for receiving control messages,
-- and one for regular messages with a payload.
data StdBoxes msg = StdBoxes {
    controlBox :: Mailbox ControlMsg,
    messageBox :: Mailbox msg
}

-- | Instantiate a 'StdBoxes' object with two 'MailBox'es.
stdBoxes :: (Actor st) (StdBoxes msg)
stdBoxes = do
    ctlBox <- mailbox
    msgBox <- mailbox
    return $ StdBoxes ctlBox msgBox

-- | A special type of message for providing control information to a 'Listener'.
--
-- The special value 'Quit' tells the 'Listener' to stop.
data ControlMsg = Quit | Info Text


-- * Actors and Message Handlers

data Context st = Context {
    act_initState  :: st,          -- ^initial state
    act_behaviours :: [Behaviour st],
    act_listener   :: Listener st,
    act_children   :: [Mailbox ControlMsg]
}

defContext state behvs children = Context state behvs defListener children
minimalContext = defContext () [] []
stdContext boxes handler state children = defContext state [
        Behv (controlBox boxes) (defCtlHandler children),
        Behv (messageBox boxes) handler
      ] children

newtype Actor st a = Actor (StateT (Context st) IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

runActor :: Actor st a -> Context st -> IO a
runActor (Actor act) ctx = evalStateT act ctx

-- | A 'Behaviour' is a combination of a 'Mailbox' and a 'MsgHandler'.
-- The 'MsgHandler' will process a message delivered via the 'Mailbox'.
data Behaviour st = forall a. Behv (Mailbox a) (MsgHandler st a)

-- | ... in case you prefer the American spelling.
type Behavior = Behaviour

-- | A message handler is called with the current state of the 'Listener'
-- and a message. It processes the message and returns 
-- a new state value wrapped in 'Just'. 
-- Returns 'Nothing' if the listener should stop receiving.
type MsgHandler st a = st -> a -> (Actor st) (Maybe st)

-- | A 'Listener' constitutes the central receive and processing loop of an actor.
--
-- It usually consists of two or more 'Behaviour's 
-- (with corresponding 'Mailboxes's and 'MsgHandler's) and a state
-- that may be changed during processing.
--
-- Use an empty list of 'Behaviour's for 'Listeners's that do not receive
-- any 'Message's.
-- Use '()' as dummy value if there is no state.
type Listener st = [Behaviour st] -> st -> (Actor st) ()

-- | Fork a new process using an 'Actor' function with the 'Behaviour's 
-- and initial state given.
spawnActor :: Context st -> Listener st -> [Behaviour st] -> st -> (Actor st) ()
spawnActor context listener behaviours state = do
    liftIO $ forkIO $ runActor (listener behaviours state) context
    return ()

-- * Predefined Actors, Listeners and Handlers

-- | A simple receive loop that stops when one of the handlers
-- invoked returns 'Nothing' instead of 'Just' a new state value.
defListener :: Listener st
defListener behvs = whileDataM $ \state -> receive state behvs

-- | A default handler for control messages, returning 'Nothing' when
-- called with a 'Quit'.
--
-- The first parameter is a list of mailboxes of
-- child actors that should also receive the control messages.
defCtlHandler :: [Mailbox ControlMsg] -> MsgHandler st ControlMsg
defCtlHandler children state Quit =
    forward children state Quit >> return Nothing
defCtlHandler children state msg =
    forward children state msg

-- | Fork a simple standard 'Actor' process with two standard 'Mailbox'es 
-- that are created during the call. 
-- Uses 'defListener' for the receive loop.
-- Returns a 'StdBoxes' object with the mailboxes created.
--
-- The first parameter is a list of mailboxes of
-- child actors that should also receive the control messages.
spawnStdActor :: Context st -> [Mailbox ControlMsg] -> MsgHandler st msg -> st 
              -> (Actor st) (StdBoxes msg)
spawnStdActor context children handler state = do
    boxes <- stdBoxes
    liftIO $ forkIO $ runActor (stdListener boxes children handler state) context
    return boxes

-- | A receive loop listening on the 'StdBoxes' given.
stdListener :: StdBoxes msg -> [Mailbox ControlMsg] -> MsgHandler st msg -> st 
            -> (Actor st) ()
stdListener boxes children handler state = 
    defListener [
        Behv (controlBox boxes) (defCtlHandler children),
        Behv (messageBox boxes) handler
      ] state

-- | A 'MsgHandler' that just sends the message received 
-- to the 'Mailbox'es given. Lets the state unchanged.
forward :: [Mailbox a] -> MsgHandler st a
forward boxes state msg = do
    forM boxes $ \box -> send box msg
    return (Just state)


-- * Messaging Functions

-- | Create a new 'Mailbox'.
mailbox :: (Actor st) (Mailbox a)
mailbox = liftIO $ atomically newTChan

-- | Send a 'Message' to a 'Mailbox'.
send :: Mailbox a -> a -> (Actor st) ()
send mb msg = liftIO $ atomically $ writeTChan mb msg

-- | Check for each of the 'Behaviour's specified if there is a 
-- 'Message' waiting in the 'Mailbox' and process the first one found. 
-- If there aren't any messages it blocks until one is available.
receive :: st -> [Behaviour st] -> (Actor st) (Maybe st)
receive state behvs =
    join (liftIO $ atomically $ processBehv state behvs)
  where
      processBehv :: st -> [Behaviour st] -> STM ((Actor st) (Maybe st))
      processBehv _ [] = retry
      processBehv state (Behv mb handler : rest) =
        tryReadTChan mb >>= \case
            Nothing -> processBehv state rest
            Just msg -> return (handler state msg)

-- | Wait for a 'Message' in the 'Mailbox' given. This function should
-- only be used in very special cases (or for testing); 
-- in most cases you will want to use 'receive' instead.
receiveMailbox :: Mailbox a -> Actor st a
receiveMailbox = liftIO . atomically . readTChan


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
