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
  Mailbox, Mailboxes, StdBoxes, ControlMsg (..),
  controlBox, messageBox, stdBoxes,
  -- * Actors and Actor Contexts
  Actor, Context (..), 
  ctxAddChild, ctxGet, ctxGets, ctxModify, ctxPut, 
  defContext, minimalContext, setDefContext, setStdContext, stdContext, 
  runActor, spawnActor, spawnDefActor, spawnStdActor,
  -- * Listeners and Message Handlers
  Behaviour (..), Behavior, Listener, MsgHandler, 
  dummyHandler, defCtlHandler, defListener, forward, stdBehvs,
  -- * Basic Messaging Functions
  call, mailbox, send, receive, receiveMessage,
  -- * Utility Functions
  whileDataM
  ) where

import BasicPrelude

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM (
    STM, TChan,
    atomically, newTChan, readTChan, tryReadTChan, writeTChan)
import Control.Monad.STM (retry)
import Control.Monad.Trans.State (
    StateT, 
    evalStateT, get, gets, modify, put, runStateT)


-- * Mailbox Types

-- | A 'Mailbox' is a typed channel in which messages are stored,
-- waiting to be read via a 'receive' or 'receiveMailbox' call.
type Mailbox a = TChan a

-- | A typical 'Actor' needs two 'Mailbox'es, one for receiving control messages,
-- and one for regular messages with a payload. The 'Mailboxes' class
-- allows standardized access to these two mailboxes also in cases
-- an actor uses additional mailboxes.
class Mailboxes bx where
    -- | Provide the mailbox for control messages.
    controlBox :: bx a -> Mailbox ControlMsg
    -- | Provide the mailbox for regular messages.
    messageBox :: bx a -> Mailbox a

-- | Standard set of mailboxes.
data StdBoxes msg = StdBoxes {
    std_ctlBox :: Mailbox ControlMsg,
    std_msgBox :: Mailbox msg
}

instance Mailboxes StdBoxes where
    controlBox = std_ctlBox
    messageBox = std_msgBox

-- | Instantiate a 'StdBoxes' object with two 'MailBox'es.
stdBoxes :: Actor st (StdBoxes msg)
stdBoxes = do
    ctlBox <- mailbox
    msgBox <- mailbox
    return $ StdBoxes ctlBox msgBox

-- | A special type of message for providing control information to a 'Listener'.
--
-- The special value 'Quit' tells the 'Listener' to stop.
data ControlMsg = Quit | Info Text


-- * Actors, Listeners, Message Handlers

-- | A 'Context' provides the properties of the receive and processing 
-- loop of an actor.
--
-- It usually consists of two or more 'Behaviour's 
-- (with corresponding 'Mailboxes's and 'MsgHandler's) and an 
-- initial state (i.e. a state that may be changed
-- by the listener during processing.
--
-- Use an empty list of 'Behaviour's for 'Listeners's that do not receive
-- any 'Message's.
-- Use '()' as dummy value if there is no state.
data Context st = Context {
    act_initState  :: st,                   -- ^ Initial state.
    act_behaviours :: [Behaviour st],       -- ^ List of behaviours.
    act_children   :: [Mailbox ControlMsg]  -- ^ Children's control mailboxes.
}

-- | A minimal 'Context' for an 'Actor' with no state, 
-- no behaviours, no children.
minimalContext :: Context ()
minimalContext = defContext () [] []

-- | A default 'Context' for an 'Actor' with the state,
-- behaviours, and children given.
defContext :: st -> [Behaviour st] -> [Mailbox ControlMsg] -> Context st
defContext state behvs children = Context state behvs children

-- | Update the current context, setting initial state and behaviours
-- to the values given.
setDefContext :: st -> [Behaviour st] -> Actor st ()
setDefContext state behvs = ctxModify $ \ctx ->
    ctx { act_initState = state, act_behaviours = behvs }

-- | Set up a new standard 'Context' with two mailboxes ('StdBoxes').
-- using the message handler and initial state given.
stdContext :: MsgHandler stn a -> stn -> Actor st (StdBoxes a, Context stn)
stdContext handler state = do
    boxes <- stdBoxes
    let ctx = defContext state (stdBehvs boxes handler []) []
    return (boxes, ctx)

-- | Update the current context using the message handler and 
-- initial state given.
setStdContext :: MsgHandler st a -> st -> Actor st (StdBoxes a)
setStdContext handler state = do
    boxes <- stdBoxes 
    setDefContext state (stdBehvs boxes handler [])
    return boxes

    -- | The 'Actor' monad that provides access to actor config data
-- via the wrapped 'Context' object.
newtype Actor st a = Actor (StateT (Context st) IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

-- | Run an 'Actor' providing it with the 'Context' given.
runActor :: Actor st a -> Context st -> IO a
runActor (Actor act) ctx = evalStateT act ctx

-- | Retrieve the 'Context' of the current 'Actor' monad.
ctxGet :: Actor st (Context st)
ctxGet = Actor get

-- | Retrieve a field of the 'Context' of the current 'Actor' monad.
ctxGets :: (Context st -> a) -> Actor st a
ctxGets = Actor . gets

-- | Store the 'Context' given in the 'Actor' monad.
ctxPut :: Context st -> Actor st ()
ctxPut st = Actor $ put st

-- | Modify the 'Context' of the current 'Actor' monad.
ctxModify :: (Context st -> Context st) -> Actor st ()
ctxModify = Actor . modify

-- | Add a child control mailbox to the set of children 
-- in the current 'Actor' monad.
ctxAddChild :: Mailbox ControlMsg -> Actor st ()
ctxAddChild mb = ctxModify $ \ctx ->
                      ctx { act_children = mb : (act_children ctx) }

-- | A 'Behaviour' is a combination of a 'Mailbox' and a 'MsgHandler'.
-- The 'MsgHandler' will process a message delivered via the 'Mailbox'.
data Behaviour st = forall a. Behv (Mailbox a) (MsgHandler st a)

-- | ... in case you prefer the American spelling.
type Behavior = Behaviour

-- | The standard 'Behaviours' setting for the two 
-- standard 'Mailboxes'.
stdBehvs :: Mailboxes bx
         => bx a            -- ^ Mailboxes, i.e. a 'Mailboxes' instance.
         -> MsgHandler st a -- ^ The message handler for the regular mailbox.
         -> [Behaviour st]  -- ^ A list of additional behaviours, maybe empty.
         -> [Behaviour st]  -- ^ The resulting list of behaviours.
stdBehvs boxes handler addBehvs = 
    Behv (controlBox boxes) defCtlHandler :
    Behv (messageBox boxes) handler : 
    addBehvs

-- | A message handler is called with the current state of the 'Listener'
-- and a message. 
--
-- It processes the message and returns 
-- a new state value wrapped in 'Just'. 
-- Returns 'Nothing' if the listener should stop receiving.
type MsgHandler st a = st -> a -> (Actor st) (Maybe st)

-- | A 'Listener' is the central receive and processing loop of an actor.
type Listener st = Actor st ()

-- | Spawn an 'Actor' in a new thread using 
-- the 'Listener' and the 'Context' given.
spawnActor :: Listener stn -> Context stn -> Actor st ()
spawnActor listener context = do
    liftIO $ forkIO $ runActor listener context
    return ()

-- | Spawn an 'Actor' using the default listener 'defListener'
spawnDefActor :: Context stn -> Actor st ()
spawnDefActor = spawnActor defListener

-- | Spawn an 'Actor' using a standard set of 'Mailboxes' and
-- 'Behaviour's. 
-- Return the 'StdBoxes' object created.
spawnStdActor :: MsgHandler stn a      -- ^ Message handler for regular mailbox.
              -> stn                   -- ^ Initial state.
              -> Actor st (StdBoxes a) -- ^ Return 'StdBoxes' object created.
spawnStdActor handler state = do
    (boxes, ctx) <- stdContext handler state
    ctxAddChild (controlBox boxes)
    spawnDefActor ctx
    return boxes


-- * Predefined Actors, Listeners and Handlers

-- | A simple receive loop that stops when one of the handlers
-- invoked returns 'Nothing' instead of 'Just' a new state value.
defListener :: Listener st
defListener = do
    initState <- ctxGets act_initState
    behvs <- ctxGets act_behaviours
    (whileDataM $ \state -> receive state behvs) initState

-- | A default handler for control messages, returning 'Nothing' when
-- called with a 'Quit' message.
defCtlHandler :: MsgHandler st ControlMsg
defCtlHandler state msg = do
    children <- ctxGets act_children
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

-- | A dummy message handler that ignores all messages.
dummyHandler :: MsgHandler st a
dummyHandler st msg = return $ Just st


-- * Messaging Functions

-- | Create a new 'Mailbox'.
mailbox :: Actor st (Mailbox a)
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

-- | Wait for a 'Message' in the 'Mailbox' given.
receiveMessage :: Mailbox a -> Actor st a
receiveMessage = liftIO . atomically . readTChan

-- | Emulates a synchronous call: send a message to an actor 
-- (addressed by a 'Mailboxes' object) and wait for a response.
--
-- The second parameter is a partial message that accepts a
-- mailbox that will receive the response. 
--
-- Use this function with care as it will block if there is no actor 
-- listening or the actor does not respond.
call :: Mailboxes mbx => mbx b -> (Mailbox r -> b) -> Actor st r
call bx dc = do
    mb <- mailbox
    send (messageBox bx) (dc mb)
    receiveMessage mb
    --let act = send (messageBox bx) (dc mb) >> receiveMessage mb
    --runActor act minimalContext


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
