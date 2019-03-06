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
  ctxGet, defContext, minimalContext, stdContext, runActor,
  -- * Listeners and Message Handlers
  Behaviour (..), Behavior, Listener, MsgHandler, 
  forward, spawnActor, defCtlHandler, defListener,
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
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)


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
stdBoxes :: IO (StdBoxes msg)
stdBoxes = do
    ctlBox <- mailbox
    msgBox <- mailbox
    return $ StdBoxes ctlBox msgBox

-- | A special type of message for providing control information to a 'Listener'.
--
-- The special value 'Quit' tells the 'Listener' to stop.
data ControlMsg = Quit | Info Text


-- * Actors, Listeners, Message Handlers

-- | A 'Context' constitutes the properties of the receive and processing 
-- loop of an actor.
--
-- It usually consists of two or more 'Behaviour's 
-- (with corresponding 'Mailboxes's and 'MsgHandler's) and a state
-- that may be changed during processing.
--
-- Use an empty list of 'Behaviour's for 'Listeners's that do not receive
-- any 'Message's.
-- Use '()' as dummy value if there is no state.
data Context st = Context {
    act_initState  :: st,          -- ^initial state
    act_behaviours :: [Behaviour st],
    act_listener   :: Listener st,
    act_children   :: [Mailbox ControlMsg]
}

defContext state behvs children = Context state behvs defListener children
minimalContext = defContext () [] []
stdContext handler state children = do
      boxes <- liftIO stdBoxes
      let ctx = defContext state [
            Behv (controlBox boxes) defCtlHandler,
            Behv (messageBox boxes) handler
            ] children
      return (boxes, ctx)

newtype Actor st a = Actor (ReaderT (Context st) IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

runActor :: Actor st a -> Context st -> IO a
runActor (Actor act) ctx = runReaderT act ctx

ctxGet :: (Context st -> a) -> Actor st a
ctxGet = Actor . asks

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

-- | A 'Listener' is the central receive and processing loop of an actor.
type Listener st = Actor st ()

-- | Fork a new process using an 'Actor' function with the 'Behaviour's 
-- and initial state given.
spawnActor :: Context st -> Listener st -> IO ()
spawnActor context listener = do
    liftIO $ forkIO $ runActor listener context
    return ()

-- * Predefined Actors, Listeners and Handlers

-- | A simple receive loop that stops when one of the handlers
-- invoked returns 'Nothing' instead of 'Just' a new state value.
defListener :: Listener st
defListener = do
    initState <- ctxGet act_initState
    behvs <- ctxGet act_behaviours
    (whileDataM $ \state -> receive state behvs) initState

-- | A default handler for control messages, returning 'Nothing' when
-- called with a 'Quit'.
defCtlHandler :: MsgHandler st ControlMsg
defCtlHandler state msg = do
    children <- ctxGet act_children
    let r = forward children state msg
    case msg of
      Quit -> return Nothing
      _    -> r

-- | A 'MsgHandler' that just sends the message received 
-- to the 'Mailbox'es given. Lets the state unchanged.
forward :: [Mailbox a] -> MsgHandler st a
forward boxes state msg = do
    forM boxes $ \box -> send box msg
    return (Just state)


-- * Messaging Functions

-- | Create a new 'Mailbox'.
mailbox :: IO (Mailbox a)
mailbox = atomically newTChan

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
